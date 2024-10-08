#nowarn "25"

open Amazon
open Amazon.EC2
open SnapshotArgs
open System
open System.IO
open Workflow
open WorkScripts.Library.Credentials
open WorkScripts.Library.EC2

let safeErrPrint (text: string) = System.Console.Error.WriteLine(text)

let locateInstance credentials (regionList: RegionEndpoint list) instanceName =
    async {
        let! locationPairList =
            [ for region in regionList ->
                async {
                    let client = new AmazonEC2Client(credentials, region)
                    let! instanceResult = getInstanceByName client instanceName

                    match instanceResult with
                    | Error _ -> return None
                    | Ok instance -> return Some(region, instance)
                } ]
            |> Async.Parallel
            >>= Seq.choose id
            >>= Seq.toList

        match locationPairList with
        | [ locationPair] ->
            return Ok locationPair
        | (_, instance) :: _ ->
            safeErrPrint $"{displayName instance} has been found in multiple regions"
            return Error(MultipleInstancesFound $"Instance {displayName instance} found in multiple regions")
        | [] ->
            let formattedRegions = regionList |> List.map _.DisplayName |> List.reduce (sprintf "%s, %s")
            safeErrPrint $"'{instanceName}' not found in regions {formattedRegions}"
            return Error(InstanceNotFound $"Instance '{instanceName}' not found in regions: {formattedRegions}")
    }

let executeSnapshots credentials args instanceLocationResults =
    let snapshotResults =
        [ for (region : RegionEndpoint, instance) in (instanceLocationResults |> Seq.choose Result.toOption) ->
              async {
                  let endpoint = RegionEndpoint.GetBySystemName(region.SystemName)
                  let client = new AmazonEC2Client(credentials, endpoint)

                  return! snapshotWorkflow args client (displayName instance)
              } ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.toList

    let errors =
        let filterErrors list = list |> Seq.filter (Result.isError)
        [ for (Error locatorError) in filterErrors instanceLocationResults -> locatorError
          for (Error snapshotError) in filterErrors snapshotResults -> snapshotError ]

    match errors with
    | [] -> Ok()
    | _ -> Error errors

[<EntryPoint>]
let main args =
    try
        let parsedArgs = cliParser.Parse args
        let awsProfile = parsedArgs.GetResult Profile

        match useLocalCredentials awsProfile with
        | None -> safeErrPrint $"Falied to get credentials with profile '{awsProfile}'. Make sure that it exists"
        | Some credentials ->
            let regionList =
                parsedArgs.GetResult Regions
                |> Seq.map RegionEndpoint.GetBySystemName
                |> Seq.toList

            let ec2LocationResults =
                    parsedArgs.GetResult Input
                    |> File.ReadAllLines
                    |> Seq.map _.Trim()
                    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                    |> Seq.map (locateInstance credentials regionList)
                    |> Async.Parallel
                    |> Async.RunSynchronously

            // Pause if errors and no ignore flag
            let ec2LocationErrors = ec2LocationResults |> Seq.filter (Result.isError)
            let ignoreErrors = parsedArgs.Contains Ignore_Errors
            let containsErrors = ec2LocationErrors |> (Seq.isEmpty >> not)

            if (containsErrors, ignoreErrors) = (true, false) then
                let boundary = String('-', 30)
                eprintfn "\n\n%sAll Errors%s" boundary boundary
                ec2LocationErrors |> Seq.iter (sprintf "%A" >> safeErrPrint)
                failwith "Stopping script. Errors found"

            // Execute snapshots
            let snapshotResults = executeSnapshots credentials parsedArgs ec2LocationResults

            match snapshotResults with
            | Ok _ -> ()
            | Error errs ->
                let boundary = String('-', 30)
                eprintfn "\n\n%sAll Errors%s" boundary boundary
                errs |> List.iter (sprintf "%A" >> safeErrPrint)


        0
    with err ->
        eprintf $"{err.Message}"
        1
