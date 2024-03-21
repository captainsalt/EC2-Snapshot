open Amazon.Runtime.CredentialManagement
open Amazon.Runtime
open Amazon.EC2
open WorkScripts.Library.EC2
open System
open System.IO

let print (input: string) = System.Console.WriteLine(input)

let snapshotWorkflow ec2Client instanceNames = 
    let ( >>= ) computation fn = 
        async {
            let! result = computation

            match result with 
            | Ok r -> return! fn r
            | Error s -> return Error s
        }

    let workflow instanceName = 
        getInstanceByName ec2Client instanceName
        >>= fun instance ->
            print $"Stopping {instance.InstanceId}"
            stopInstance ec2Client instance
        >>= fun instance ->
            let changeRequestNumber = ""

            let request = {
                instance = instance
                amiName = $"{instanceName}-{changeRequestNumber}"
                description = "3/21/2024 {changeRequestNumber}"
                tags = [
                    "Name", instanceName
                    "InstanceID", instance.InstanceId
                    "SNOW-TICKET", changeRequestNumber
                ]
            }

            print $"""Creating ami for {instance.InstanceId}"""
            createAmi ec2Client request
        >>= fun instance -> 
            print $"Starting {instance.InstanceId}"
            startInstance ec2Client instance

    instanceNames 
    |> Seq.map (fun instanceName -> workflow instanceName)
    |> Async.Parallel

let createEC2ClientWithProfile profileName =
    let chain = CredentialProfileStoreChain()
    let mutable credentials = Unchecked.defaultof<AWSCredentials>
    if chain.TryGetAWSCredentials(profileName, &credentials) then
        Ok (new AmazonEC2Client(credentials, Amazon.RegionEndpoint.USGovCloudEast1))
    else 
        Error $"Failed to create EC2 client. Profile not found: {profileName}"

// Example usage
let profileName = "lab"
let ec2ClientResult = createEC2ClientWithProfile profileName

match ec2ClientResult with
| Ok ec2Client ->
    try
        let names = File.ReadAllLines "Instances.txt"
        let results = snapshotWorkflow ec2Client names |> Async.RunSynchronously
        printfn "Results: %A" results
    with
    | ex ->
        eprintf "Unexpected error: %s" ex.Message
| Error errorMsg ->
    printfn "Error creating client: %s" errorMsg
