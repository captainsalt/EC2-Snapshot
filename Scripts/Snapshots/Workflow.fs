module Workflow

open Argu
open SnapshotArgs
open WorkScripts.Library.EC2

let private print (input: string) = System.Console.WriteLine(input)

let private (>>=) computation fn =
    async {
        match! computation with
        | Ok r -> return! fn r
        | Error s -> return Error s
    }
    
let snapshotWorkflow (parsedArgs: ParseResults<Arguments>) ec2Client instanceName =
    getInstanceByName ec2Client instanceName
    >>= fun instance ->
        match parsedArgs.Contains Stop_Instances with
        | true ->
            print $"Stopping {displayName instance}"
            stopInstance ec2Client instance
        | false -> async.Return(Ok instance)

    >>= fun instance ->
        let changeTaskNumber = parsedArgs.GetResult(CTask)

        let amiRequest =
            {   instance = instance
                amiName = $"{instanceName}-{changeTaskNumber}"
                description = parsedArgs.GetResult(Description)
                tags =
                    [
                      "Name", instanceName
                      "InstanceID", instance.InstanceId
                      "SNOW-TICKET", changeTaskNumber ] }

        print $"""Creating ami for {displayName instance}"""

        createAmi ec2Client amiRequest

    >>= fun instance ->
        match parsedArgs.Contains Start_Instances with
        | true ->
            print $"Starting {displayName instance}"
            startInstance ec2Client instance
        | false -> async.Return(Ok instance)
