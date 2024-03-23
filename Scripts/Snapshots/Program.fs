open WorkScripts.Library.Credentials
open WorkScripts.Library.EC2
open System.IO
open Workflow
open Amazon.EC2
open SnapshotArgs

let locateInstance awsProfile instanceName = 
    let regions = Amazon.RegionEndpoint.EnumerableAllRegions

    [ for region in regions do 
        useLocalCredentials "lab" 
        |> Result.map 
            (fun creds -> 
                let client = new AmazonEC2Client(creds, region)
                getInstanceByName client instanceName
            )
    ]

[<EntryPoint>]
let main args = 
    locateInstance "" ""

    // printfn "%A" (cliParser.Parse args)
    0