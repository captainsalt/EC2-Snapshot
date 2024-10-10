namespace WorkScripts.Library

module Credentials =
    open Amazon.Runtime
    open Amazon.Runtime.CredentialManagement

    let useLocalCredentials profileName =
        let chain = CredentialProfileStoreChain()
        let mutable credentials = Unchecked.defaultof<AWSCredentials>

        if chain.TryGetAWSCredentials(profileName, &credentials) then
            Some credentials
        else
            None

module EC2 =
    open Amazon.EC2
    open Amazon.EC2.Model
    open System.Collections.Generic

    type AmiRequest =
        { amiName: string
          description: string
          tags: (string * string) seq
          instance: Instance }

    type Error =
        | InstanceNotFound of string
        | ImageNotFound of string
        | InstanceTerminated of string
        | InstanceNotStopped of string
        | MultipleInstancesFound of string
        | ErrorStoppingInstance of string
        | ErrorStartingInstance of string
        | ErrorCreatingImage of string

    let ( >>>= ) m fn = async.Bind(m, fn)

    let ( >>= ) m fn = async.Bind(m, fn >> async.Return)

    let private tagsFromTuple (tags: (string * string) seq) =
        tags |> Seq.map (fun (name, value) -> new Tag(name, value))

    let private createDescribeInstancesRequest filterName filterValue =
        new DescribeInstancesRequest(
            Filters = new List<Filter>([ new Filter(filterName, new List<string>([ filterValue ])) ])
        )

    let private extractInstances (response: DescribeInstancesResponse) =
        response.Reservations |> Seq.collect _.Instances |> Seq.toList

    let rec private waitForImage (ec2Client: AmazonEC2Client) (imageId: string) =
        async {
            let! image =
                DescribeImagesRequest(ImageIds = new List<string>([ imageId ]))
                |> ec2Client.DescribeImagesAsync
                |> Async.AwaitTask
                >>= fun describeResponse -> describeResponse.Images
                                            |> Seq.tryHead

            let isInvalidState (image: Image) =
                List.contains image.State [ ImageState.Error; ImageState.Failed; ImageState.Invalid ]

            let isAvaliable (image: Image) = image.State.Value = ImageState.Available.Value

            match image with
            | Some image when isAvaliable image -> return Ok image.ImageId
            | Some image when isInvalidState image ->
                return Error(ErrorCreatingImage $"Error creating image for instance {image.SourceInstanceId}")
            | Some image ->
                do! Async.Sleep 2_500
                return! waitForImage ec2Client image.ImageId
            | None -> return Error(ImageNotFound $"Image {imageId} not found")
        }

    let private isInstanceStopped (ec2Client: AmazonEC2Client) (instance: Instance) =
        async {
            let! instance =
                createDescribeInstancesRequest "instance-id" instance.InstanceId
                |> ec2Client.DescribeInstancesAsync
                |> Async.AwaitTask
                >>= fun response -> extractInstances response
                                    |> Seq.tryHead

            match instance with
            | Some instance when instance.State.Name = InstanceStateName.Stopped -> return true
            | Some _ -> return false
            | None -> return false
        }

    let displayName (instance: Instance) =
        instance.Tags
        |> Seq.tryFind (fun t -> t.Key = "Name")
        |> Option.map(fun t -> t.Value)
        |> Option.defaultValue(instance.InstanceId)

    let getInstanceById (ec2Client: AmazonEC2Client) (instanceId: string) =
        async {
            let! instances =
                createDescribeInstancesRequest "instance-id" instanceId
                |> ec2Client.DescribeInstancesAsync
                |> Async.AwaitTask
                >>= fun response -> extractInstances response

            match instances with
            | [ instance ] when instance.State.Name = InstanceStateName.Terminated ->
                return Error(InstanceTerminated $"Instance '{displayName instance}' has been terminated")
            | [ instance ] -> return Ok instance
            | _ -> return Error(InstanceNotFound $"Instance with id '{instanceId}' not found")
        }

    let getInstanceByName (ec2Client: AmazonEC2Client) (nameTag: string) =
        async {
            let! instances =
                createDescribeInstancesRequest "tag:Name" nameTag
                |> ec2Client.DescribeInstancesAsync
                |> Async.AwaitTask
                >>= fun response -> extractInstances response
                >>= fun instances -> instances |> List.filter (fun i -> i.State.Name <> InstanceStateName.Terminated)

            match instances with
            | [ instance ] -> return Ok instance
            | [] -> return Error(InstanceNotFound $"Instance with name '{nameTag}' not found")
            | instances ->
                let instanceIds = instances |> List.map _.InstanceId |> List.reduce (sprintf "%s, %s")

                return
                    Error(
                        MultipleInstancesFound
                            $"Multiple instances found with tag name '{nameTag}'. Their ids are: {instanceIds}"
                    )
        }

    let stopInstance (ec2Client: AmazonEC2Client) (instance: Instance) =
        async {
            do!
                ec2Client.StopInstancesAsync(StopInstancesRequest(InstanceIds = ResizeArray [ instance.InstanceId ]))
                |> Async.AwaitTask
                |> Async.Ignore

            let rec waitUntilStopped () =
                async {
                    let! instanceResult = getInstanceById ec2Client instance.InstanceId

                    match instanceResult with
                    | Ok instance when instance.State.Name = InstanceStateName.Stopped -> return Ok instance
                    | Ok _ ->
                        do! Async.Sleep 2_500
                        return! waitUntilStopped ()
                    | Error s ->
                        return
                            Error(ErrorStoppingInstance $"Error while stopping instance '{displayName instance}': {s}")
                }

            return! waitUntilStopped ()
        }

    let createAmi (ec2Client: AmazonEC2Client) (amiRequest: AmiRequest) =
        async {
            match! isInstanceStopped ec2Client amiRequest.instance with
            | false ->
                return Error(InstanceNotStopped $"Instance {displayName amiRequest.instance} has not been stopped")
            | true ->
                let tags = tagsFromTuple amiRequest.tags

                let imageRequest =
                    new CreateImageRequest(
                        InstanceId = amiRequest.instance.InstanceId,
                        Name = amiRequest.amiName,
                        Description = amiRequest.description,
                        TagSpecifications =
                            new List<TagSpecification>(
                                [ new TagSpecification(ResourceType = ResourceType.Image, Tags = new List<Tag>(tags))
                                  new TagSpecification(ResourceType = ResourceType.Snapshot, Tags = new List<Tag>(tags)) ]
                            )
                    )

                let! image =
                    ec2Client.CreateImageAsync(imageRequest)
                    |> Async.AwaitTask
                    >>>= fun response -> waitForImage ec2Client response.ImageId

                match image with
                | Ok _ -> return Ok amiRequest.instance
                | Error err -> return Error err
        }

    let startInstance (ec2Client: AmazonEC2Client) (instance: Instance) =
        async {
            do!
                ec2Client.StartInstancesAsync(StartInstancesRequest(InstanceIds = ResizeArray [ instance.InstanceId ]))
                |> Async.AwaitTask
                |> Async.Ignore

            let rec waitUntilStarted () =
                async {
                    let! instanceResult = getInstanceById ec2Client instance.InstanceId

                    match instanceResult with
                    | Ok instance when instance.State.Name = InstanceStateName.Running -> return Ok instance
                    | Ok _ ->
                        do! Async.Sleep 2_500
                        return! waitUntilStarted ()
                    | Error msg ->
                        return
                            Error(ErrorStartingInstance $"Error when starting instance '{displayName instance}': {msg}")
                }

            return! waitUntilStarted ()
        }
