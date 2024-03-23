module Workflow

open SnapshotArgs
open Amazon.EC2.Model
open WorkScripts.Library.EC2

let private print (input: string) = System.Console.WriteLine(input)

let private ( >>= ) computation fn = 
    async {
        match! computation with 
        | Ok r -> return! fn r
        | Error s -> return Error s
    }

let displayName (instance: Instance) = 
    let nameTag = 
        instance.Tags 
        |> Seq.tryFind (fun t -> t.Key = "Name")

    match nameTag with 
    | Some name -> name.Value
    | None -> instance.InstanceId

let snapshotWorkflow arguments ec2Client instanceName = 
    let cliArguments = cliParser.Parse arguments

    getInstanceByName ec2Client instanceName
    >>= fun instance ->
        match cliArguments.Contains Stop_Instances with 
        | true -> 
            print $"Stopping {displayName instance}"
            stopInstance ec2Client instance
        | false -> async.Return (Ok instance)
    >>= fun instance ->
        let changeRequestNumber = cliArguments.GetResult(Task)

        let amiRequest = {
            instance = instance
            amiName = $"{instanceName}-{changeRequestNumber}"
            description = cliArguments.GetResult(Description)
            tags = [
                "Name", instanceName
                "InstanceID", instance.InstanceId
                "SNOW-TICKET", changeRequestNumber
            ]
        }

        print $"""Creating ami for {displayName instance}"""
        createAmi ec2Client amiRequest
    >>= fun instance -> 
        match cliArguments.Contains Start_Instances with 
        | true -> 
            print $"Starting {displayName instance}"
            startInstance ec2Client instance
        | false -> 
            async.Return (Ok instance)

    