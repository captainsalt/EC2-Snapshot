module AwsInterface

open Amazon.EC2.Model

let private tagsFromTuple (tags: (string * string) seq) =
    tags |> Seq.map (fun (name, value) -> new Tag(name, value))

module EC2 =
    open Amazon.EC2
    open System.Collections.Generic

    type AmiRequest =
        { amiName: string
          description: string
          tags: (string * string) seq
          instanceId: string }

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
            | Some image when image.State.Value = ImageState.Available.Value -> return Ok "Image created"
            | Some image when invalidStates |> List.contains image.State ->
                return Error $"Error creating image for instance {image.SourceInstanceId}"
            | Some image ->
                do! Async.Sleep 5_000
                return! waitForImage ec2Client image.ImageId
            | None -> return Error "Image not found"
        }

    let getInstanceById (ec2Client: AmazonEC2Client) (instanceId: string) =
        async {
            let request = createDescribeInstancesRequest "instance-id" instanceId
            let! response = ec2Client.DescribeInstancesAsync(request) |> Async.AwaitTask
            let instances = extractInstances response

            match instances with
            | [ instance ] -> return Ok instance
            | _ -> return Error $"Instance with id '{instanceId}' not found"
        }

    let getInstanceByName (ec2Client: AmazonEC2Client) (nameTag: string) =
        async {
            let request = createDescribeInstancesRequest "tag:Name" nameTag
            let! response = ec2Client.DescribeInstancesAsync(request) |> Async.AwaitTask
            let instances = extractInstances response

            match instances with
            | [ instance ] -> return Ok instance
            | [] -> return Error $"Instance with name '{nameTag}' not found"
            | instances ->
                let instanceIds =
                    instances |> List.map _.InstanceId |> List.reduce (sprintf "%s, %s")

                return Error $"Multiple instances found with tag name '{nameTag}'. Their ids are: {instanceIds}"
        }

    let stopInstance (ec2Client: AmazonEC2Client) (instanceId: string) =
        async {
            do!
                ec2Client.StopInstancesAsync(StopInstancesRequest(InstanceIds = ResizeArray [ instanceId ]))
                |> Async.AwaitTask
                |> Async.Ignore

            let rec waitUntilStopped () =
                async {
                    let! instanceResult = getInstanceById ec2Client instanceId

                    match instanceResult with
                    | Ok instance when instance.State.Name = InstanceStateName.Stopped ->
                        return Ok $"Instance {instanceId} stopped"
                    | Ok _ ->
                        do! Async.Sleep 5_000
                        return! waitUntilStopped ()
                    | Error s -> return Error $"Error while stopping instance '{instanceId}': {s}"
                }

            return! waitUntilStopped ()
        }

    let createAmi (ec2Client: AmazonEC2Client) (amiRequest: AmiRequest) =
        async {
            let tags = tagsFromTuple amiRequest.tags

            let imageRequest =
                new CreateImageRequest(
                    InstanceId = amiRequest.instanceId,
                    Name = amiRequest.amiName,
                    TagSpecifications =
                        new List<TagSpecification>(
                            [ new TagSpecification(ResourceType = ResourceType.Instance, Tags = new List<Tag>(tags)) ]
                        )
                )

            let! response = ec2Client.CreateImageAsync(imageRequest) |> Async.AwaitTask
            return! waitForImage ec2Client response.ImageId
        }

    let startInstance (ec2Client: AmazonEC2Client) (instanceId: string) =
        async {
            do!
                ec2Client.StartInstancesAsync(StartInstancesRequest(InstanceIds = ResizeArray [ instanceId ]))
                |> Async.AwaitTask
                |> Async.Ignore

            let rec waitUntilStarted () =
                async {
                    let! instanceResult = getInstanceById ec2Client instanceId

                    match instanceResult with
                    | Ok instance when instance.State.Name = InstanceStateName.Running ->
                        return Ok $"Instance {instanceId} running"
                    | Ok _ ->
                        do! Async.Sleep 5_000
                        return! waitUntilStarted ()
                    | Error msg -> return Error $"Error when starting instance '{instanceId}': {msg}"
                }

            return! waitUntilStarted ()
        }
