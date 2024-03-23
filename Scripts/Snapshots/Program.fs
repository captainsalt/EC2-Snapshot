open WorkScripts.Library.Credentials
open WorkScripts.Library.EC2
open System.IO
open Workflow
open Amazon.EC2
open Amazon.EC2.Model
open SnapshotArgs

let locateInstance awsProfile instanceName = 
    let regions = Amazon.RegionEndpoint.EnumerableAllRegions

    match useLocalCredentials awsProfile with 
    | Ok credentials ->
        [ for region in regions ->
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
        |> Seq.exactlyOne
    | Error err -> failwith err

[<EntryPoint>]
let main args = 
    let parsedArgs = cliParser.Parse args
    let awsProfile = parsedArgs.GetResult Profile
    
    // Implementation    
    let instanceIds  = parsedArgs.GetResult Instance_Ids |> File.ReadAllLines
    let instanceLocations = 
        instanceIds 
        |> Seq.map (locateInstance awsProfile)
        |> Map.ofSeq

    printfn "%A" (cliParser.Parse args)
    0