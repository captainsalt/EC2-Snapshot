module Workflow

open Argu
open SnapshotArgs
open WorkScripts.Library.EC2
open Amazon.EC2.Model

let private print (input: string) = System.Console.WriteLine(input)

let private (|IdServer|NonIdServer|) (instance: Instance) =
    match displayName instance |> _.Contains("id") with
    | true -> IdServer instance
    | false -> NonIdServer instance

let private (>>=) computation fn =
    async {
        match! computation with
        | Ok r -> return! fn r
        | Error s -> return Error s
    }

let private combineTags (instance: Instance) additionalTags =
    let instanceTags =
        [ for tag in instance.Tags -> (tag.Key, tag.Value) ] |> Map.ofList

    additionalTags
    |> List.filter (fst >> instanceTags.ContainsKey >> not)
    |> (@) (instanceTags |> Map.toList)

let snapshotWorkflow (parsedArgs: ParseResults<Arguments>) ec2Client instanceName =
    getInstanceByName ec2Client instanceName
    >>= fun instance ->
        match parsedArgs.Contains Stop_Instances with
        | false -> async.Return(Ok instance)
        | true ->
            print $"Stopping {displayName instance}"
            stopInstance ec2Client instance

    >>= fun instance ->
        let changeTaskNumber = parsedArgs.GetResult(CTask)
        let tags =
            let snapshotTags =
                [ "Name", instanceName
                  "InstanceID", instance.InstanceId
                  "SNOW-TICKET", changeTaskNumber ]

            combineTags instance snapshotTags

        let amiRequest =
            { instance = instance
              amiName = $"{instanceName}-{changeTaskNumber}"
              description = parsedArgs.GetResult(Description)
              tags = tags }

        print $"""Creating ami for {displayName instance}"""

        createAmi ec2Client amiRequest

    >>= fun instance ->
        match parsedArgs.Contains Start_Instances with
        | false -> async.Return(Ok instance)
        | true ->
            match instance with
            | NonIdServer instance -> Ok instance |> async.Return
            | IdServer instance ->
                print $"Starting {displayName instance}"
                startInstance ec2Client instance

