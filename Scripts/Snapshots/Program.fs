open WorkScripts.Library.Credentials
open WorkScripts.Library.EC2
open System.IO
open Workflow
open Amazon.EC2
open SnapshotArgs
open Amazon

let locateInstance awsProfile (regionList: RegionEndpoint list) instanceName = 
    match useLocalCredentials awsProfile with 
    | Ok credentials ->
        [ for region in regionList ->
            async {
                let client = new AmazonEC2Client(credentials, region)
                let! instanceResult = getInstanceByName client instanceName

                match instanceResult with 
                | Error (InstanceNotFound _) -> return None
                | Error err -> return failwith $"Unexpected error {err}"
                | Ok instance -> return Some (region.SystemName, instance)
            }
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.choose id
    | Error err -> failwith err

[<EntryPoint>]
let main args = 
    let parsedArgs = cliParser.Parse args
    let awsProfile = parsedArgs.GetResult SnapshotArgs.Profile
    let regionList = parsedArgs.GetResult Regions |> Seq.map RegionEndpoint.GetBySystemName |> Seq.toList
    
    // Implementation    
    let instanceIds = parsedArgs.GetResult Input |> File.ReadAllLines
    let instanceLocations =
        instanceIds 
        |> Seq.collect (locateInstance awsProfile regionList)
        |> Seq.sortBy (fun (region, _) -> region)

    match useLocalCredentials awsProfile with
    | Ok credentials ->  
        let results = 
            [ for (region, instance) in instanceLocations ->
                    async {
                        let endpoint = RegionEndpoint.GetBySystemName(region)
                        let client = new AmazonEC2Client(credentials, endpoint)
                        return! snapshotWorkflow args client (displayName instance)
                    }
            ] 
            |> Async.Parallel
            |> Async.RunSynchronously

        // Log negative results
        printfn "%A" results
    | Error err -> 
        failwith err

    0