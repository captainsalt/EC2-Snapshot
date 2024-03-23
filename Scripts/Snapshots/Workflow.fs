module Workflow

open SnapshotArgs
open Amazon.EC2.Model
open WorkScripts.Library.EC2
open System

let private print (input: string) = System.Console.WriteLine(input)

let private (>>=) computation fn =
    async {
        match! computation with
        | Ok r -> return! fn r
        | Error s -> return Error s
    }
    
let snapshotWorkflow arguments ec2Client instanceName =
    let cliArguments = cliParser.Parse arguments

    getInstanceByName ec2Client instanceName
    >>= fun instance ->
        match cliArguments.Contains Stop_Instances with
        | true ->
            print $"Stopping {displayName instance}"
            stopInstance ec2Client instance
        | false -> async.Return(Ok instance)

    >>= fun instance ->
        let changeTaskNumber = cliArguments.GetResult(CTask)

        let amiRequest =
            {   instance = instance
                amiName = $"{instanceName}-{changeTaskNumber}"
                description = cliArguments.GetResult(Description)
                tags =
                    [
                      "Name", instanceName
                      "InstanceID", instance.InstanceId
                      "SNOW-TICKET", changeTaskNumber ] }

        print $"""Creating ami for {displayName instance}"""

        createAmi ec2Client amiRequest

    >>= fun instance ->
        match cliArguments.Contains Start_Instances with
        | true ->
            print $"Starting {displayName instance}"
            startInstance ec2Client instance
        | false -> async.Return(Ok instance)
