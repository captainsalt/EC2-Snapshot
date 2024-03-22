namespace WorkScripts.Library

module Credentials =
    open Amazon.Runtime.CredentialManagement
    open Amazon.Runtime

    let getLocalCredentials profileName credentialFn = 
        let chain = CredentialProfileStoreChain()
        let mutable credentials = Unchecked.defaultof<AWSCredentials>

        if chain.TryGetAWSCredentials(profileName, &credentials) then
            credentialFn credentials |> Ok
        else
            Error $"Failed to create EC2 client. Profile not found: {profileName}"

module EC2 =
    open Amazon.EC2
    open System.Collections.Generic
    open Amazon.EC2.Model

    type AmiRequest =
        { amiName: string
          description: string
          tags: (string * string) seq
          instance: Instance }

    type Error =
        | InstanceNotFound of string
        | ImageNotFound of string
        | InstanceTerminated of string
        | MultipleInstancesFound of string
        | ErrorStoppingInstance of string
        | ErrorStartingInstance of string
        | ErrorCreatingImage of string

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
            let describeImageRequest =
                DescribeImagesRequest(ImageIds = new List<string>([ imageId ]))

            let! imageResult = ec2Client.DescribeImagesAsync(describeImageRequest) |> Async.AwaitTask
            let invalidStates = [ ImageState.Error; ImageState.Failed; ImageState.Invalid ]

            let image = imageResult.Images |> Seq.tryHead

            match image with
            | Some image when image.State.Value = ImageState.Available.Value -> return Ok image.ImageId
            | Some image when invalidStates |> List.contains image.State ->
                return Error(ErrorCreatingImage $"Error creating image for instance {image.SourceInstanceId}")
            | Some image ->
                do! Async.Sleep 3_000
                return! waitForImage ec2Client image.ImageId
            | None -> return Error(ImageNotFound $"Image {imageId} not found")
        }

    let getInstanceById (ec2Client: AmazonEC2Client) (instanceId: string) =
        async {
            let request = createDescribeInstancesRequest "instance-id" instanceId
            let! response = ec2Client.DescribeInstancesAsync(request) |> Async.AwaitTask
            let instances = extractInstances response

            match instances with
            | [ instance ] when instance.State.Name = InstanceStateName.Terminated ->
                return Error(InstanceTerminated $"Instance '{instance.InstanceId}' has been terminated")
            | [ instance ] -> return Ok instance
            | _ -> return Error(InstanceNotFound $"Instance with id '{instanceId}' not found")
        }

    let getInstanceByName (ec2Client: AmazonEC2Client) (nameTag: string) =
        async {
            let request = createDescribeInstancesRequest "tag:Name" nameTag
            let! response = ec2Client.DescribeInstancesAsync(request) |> Async.AwaitTask
            let instances = extractInstances response

            match instances with
            | [ instance ] when instance.State.Name = InstanceStateName.Terminated ->
                return Error(InstanceTerminated $"Instance with Name tag '{nameTag}' has been terminated")
            | [ instance ] -> return Ok instance
            | [] -> return Error(InstanceNotFound $"Instance with name '{nameTag}' not found")
            | instances ->
                let instanceIds =
                    instances |> List.map _.InstanceId |> List.reduce (sprintf "%s, %s")

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
                            Error(ErrorStoppingInstance $"Error while stopping instance '{instance.InstanceId}': {s}")
                }

            return! waitUntilStopped ()
        }

    let createAmi (ec2Client: AmazonEC2Client) (amiRequest: AmiRequest) =
        async {
            let tags = tagsFromTuple amiRequest.tags

            let imageRequest =
                new CreateImageRequest(
                    InstanceId = amiRequest.instance.InstanceId,
                    Name = amiRequest.amiName,
                    TagSpecifications =
                        new List<TagSpecification>(
                            [ new TagSpecification(ResourceType = ResourceType.Image, Tags = new List<Tag>(tags))
                              new TagSpecification(ResourceType = ResourceType.Snapshot, Tags = new List<Tag>(tags)) ]
                        )
                )

            let! response = ec2Client.CreateImageAsync(imageRequest) |> Async.AwaitTask
            let! imageResult = waitForImage ec2Client response.ImageId

            match imageResult with
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
                            Error(ErrorStartingInstance $"Error when starting instance '{instance.InstanceId}': {msg}")
                }

            return! waitUntilStarted ()
        }
