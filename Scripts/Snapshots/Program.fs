#nowarn "25"
open WorkScripts.Library.Credentials
open WorkScripts.Library.EC2
open System.IO
open Workflow
open Amazon.EC2
open SnapshotArgs
open Amazon

let locateInstance credentials (regionList: RegionEndpoint list) instanceName =
    let instance =
        [ for region in regionList ->
                async {
                    let client = new AmazonEC2Client(credentials, region)
                    let! instanceResult = getInstanceByName client instanceName

                    match instanceResult with
                    | Error(InstanceNotFound _) -> return None
                    | Error err -> return failwith $"Unexpected error {err}"
                    | Ok instance -> return Some(region.SystemName, instance)
                } ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.choose id
        |> Seq.toList

    match instance with
    | [ locationPair ] -> Ok locationPair
    | (_,  instance) :: _ -> 
        Error(
            MultipleInstancesFound
                $"Instance {displayName instance} found in multiple regions"
        )
    | [] -> Error (InstanceNotFound $"No instance found with the name '{instanceName}'")

let executeSnapshots credentials args instanceLocationResults  = 
    let snapshotResults =
        [ for (region, instance) in (instanceLocationResults |> Seq.choose Result.toOption) ->
            async {
                let endpoint = RegionEndpoint.GetBySystemName(region)
                let client = new AmazonEC2Client(credentials, endpoint)
                return! snapshotWorkflow args client (displayName instance)
            } ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.toList
    let errors = [
        for (Error snapshotError) in snapshotResults |> Seq.filter (Result.isError) -> 
            snapshotError

        for (Error locationError) in instanceLocationResults |> Seq.filter (Result.isError) -> 
            locationError
    ]

    match errors with 
    | [] -> Ok ()
    | _ -> Error errors

[<EntryPoint>]
let main args =
    try
        let parsedArgs = cliParser.Parse args
        let awsProfile = parsedArgs.GetResult SnapshotArgs.Profile

        match useLocalCredentials awsProfile with 
        | Ok credentials -> 
            let regionList =
                parsedArgs.GetResult Regions
                |> Seq.map RegionEndpoint.GetBySystemName
                |> Seq.toList

            // Implementation
            let instanceIds = parsedArgs.GetResult Input |> File.ReadAllLines |> Seq.map _.Trim() |> Seq.filter ((<>) "")

            let instanceLocationResults =
                instanceIds
                |> Seq.map (locateInstance credentials regionList)
       
            let snapshotResults = executeSnapshots credentials args instanceLocationResults

            match snapshotResults with 
            | Ok _ -> () 
            | Error errs -> errs |> List.iter (fun err -> eprintfn $"{err}")

        | Error err -> 
            failwith err

        0
    with
    | err -> 
        eprintf $"{err.Message}"
        1
