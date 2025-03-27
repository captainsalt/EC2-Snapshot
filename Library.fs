namespace WorkScripts.Library

module Credentials =
    open Amazon.Runtime
    open Amazon.Runtime.CredentialManagement

    /// Retrieve AWS credentials from a specific profile
    let tryGetCredentials (profileName: string) : AWSCredentials option =
        let chain = CredentialProfileStoreChain()
        match chain.TryGetAWSCredentials(profileName) with
        | true, credentials -> Some credentials
        | _ -> None

module EC2 =
    open Amazon.EC2
    open Amazon.EC2.Model
    open System.Collections.Generic

    /// Represents a request to create an Amazon Machine Image (AMI)
    type AmiRequest = {
        AmiName: string
        Description: string
        Tags: (string * string) list
        Instance: Instance
    }

    /// Comprehensive error type for EC2 operations
    type OperationError =
        | InstanceNotFound of message: string
        | ImageNotFound of message: string
        | InstanceTerminated of message: string
        | InstanceNotStopped of message: string
        | MultipleInstancesFound of message: string
        | ErrorStoppingInstance of message: string
        | ErrorStartingInstance of message: string
        | ErrorCreatingImage of message: string


    let (>>>=) m fn = async.Bind(m, fn)
    let (>>=) m fn = async.Bind(m, fn >> async.Return)

    /// Utility functions for AWS EC2 operations
    module internal Helpers =
        let tagsToAwsTags (tags: (string * string) seq) =
            tags |> Seq.map (fun (name, value) -> Tag(name, value)) |> List.ofSeq

        let createDescribeInstancesRequest filterName filterValue =
            DescribeInstancesRequest(
                Filters = ResizeArray [ Filter(filterName, ResizeArray [ filterValue ]) ]
            )

        let extractInstances (response: DescribeInstancesResponse) =
            response.Reservations 
            |> Seq.collect (fun reservation -> reservation.Instances)
            |> List.ofSeq

        let displayName (instance: Instance) =
            instance.Tags
            |> Seq.tryFind (fun t -> t.Key = "Name")
            |> Option.map (fun t -> t.Value)
            |> Option.defaultValue instance.InstanceId

    /// Waits for an AMI to become available
    let private waitForImage (ec2Client: AmazonEC2Client) (imageId: string) =
        let rec waitLoop() = async {
            let! describeResponse = 
                ec2Client.DescribeImagesAsync(DescribeImagesRequest(ImageIds = ResizeArray [ imageId ]))
                |> Async.AwaitTask

            let image = describeResponse.Images |> Seq.tryHead

            match image with
            | Some img when img.State.Value = ImageState.Available.Value -> 
                return Ok img.ImageId
            | Some img when 
                List.contains img.State [ ImageState.Error; ImageState.Failed; ImageState.Invalid ] ->
                return Error(OperationError.ErrorCreatingImage $"Error creating image for instance {img.SourceInstanceId}")
            | Some _ -> 
                do! Async.Sleep 2500
                return! waitLoop()
            | None -> 
                return Error(OperationError.ImageNotFound $"Image {imageId} not found")
        }

        waitLoop ()

    /// Check if an instance is in a stopped state
    let private isInstanceStopped (ec2Client: AmazonEC2Client) (instance: Instance) =
        async {
            let! response = 
                Helpers.createDescribeInstancesRequest "instance-id" instance.InstanceId
                |> ec2Client.DescribeInstancesAsync 
                |> Async.AwaitTask

            return 
                Helpers.extractInstances response
                |> List.tryHead
                |> Option.exists (fun i -> i.State.Name = InstanceStateName.Stopped)
        }

    /// Retrieve an instance by its ID
    let getInstanceById (ec2Client: AmazonEC2Client) (instanceId: string) =
        async {
            let! instances = 
                Helpers.createDescribeInstancesRequest "instance-id" instanceId
                |> ec2Client.DescribeInstancesAsync 
                |> Async.AwaitTask
                >>= (Helpers.extractInstances >> List.filter (fun i -> i.State.Name <> InstanceStateName.Terminated))

            match instances with
            | [ instance ] -> return Ok instance
            | [] -> return Error(OperationError.InstanceNotFound $"Instance with id '{instanceId}' not found")
            | _ -> 
                let instanceIds = instances |> List.map (fun i -> i.InstanceId) |> String.concat ", "
                return Error(OperationError.MultipleInstancesFound 
                    $"Multiple non-terminated instances found with id '{instanceId}'. Their ids are: {instanceIds}")
        }

    /// Retrieve an instance by its Name tag
    let getInstanceByName (ec2Client: AmazonEC2Client) (nameTag: string) =
        async {
            let! instances = 
                Helpers.createDescribeInstancesRequest "tag:Name" nameTag
                |> ec2Client.DescribeInstancesAsync 
                |> Async.AwaitTask
                >>= (Helpers.extractInstances >> List.filter (fun i -> i.State.Name <> InstanceStateName.Terminated))

            match instances with
            | [ instance ] -> return Ok instance
            | [] -> return Error(OperationError.InstanceNotFound $"Instance with name '{nameTag}' not found")
            | instances ->
                let instanceIds = instances |> List.map (fun i -> i.InstanceId) |> String.concat ", "
                return Error(OperationError.MultipleInstancesFound 
                    $"Multiple instances found with tag name '{nameTag}'. Their ids are: {instanceIds}")
        }

    /// Stop an EC2 instance
    let stopInstance (ec2Client: AmazonEC2Client) (instance: Instance) =
        async {
            do! ec2Client.StopInstancesAsync(StopInstancesRequest(InstanceIds = ResizeArray [ instance.InstanceId ]))
                |> Async.AwaitTask
                |> Async.Ignore

            let rec waitUntilStopped() = async {
                let! instanceResult = getInstanceById ec2Client instance.InstanceId

                match instanceResult with
                | Ok stoppedInstance when stoppedInstance.State.Name = InstanceStateName.Stopped -> 
                    return Ok stoppedInstance
                | Ok _ -> 
                    do! Async.Sleep 2500
                    return! waitUntilStopped()
                | Error msg -> 
                    return Error(OperationError.ErrorStoppingInstance 
                        $"Error while stopping instance '{Helpers.displayName instance}': {msg}")
            }

            return! waitUntilStopped()
        }

    /// Create an Amazon Machine Image (AMI) from an instance
    let createAmi (ec2Client: AmazonEC2Client) (amiRequest: AmiRequest) =
        async {
            let! isStopped = isInstanceStopped ec2Client amiRequest.Instance

            if not isStopped then 
                return Error(OperationError.InstanceNotStopped 
                    $"Instance {Helpers.displayName amiRequest.Instance} has not been stopped")
            else
                let tags = Helpers.tagsToAwsTags amiRequest.Tags

                let imageRequest = 
                    CreateImageRequest(
                        InstanceId = amiRequest.Instance.InstanceId,
                        Name = amiRequest.AmiName,
                        Description = amiRequest.Description,
                        TagSpecifications = ResizeArray [
                            TagSpecification(ResourceType = ResourceType.Image, Tags = ResizeArray tags)
                            TagSpecification(ResourceType = ResourceType.Snapshot, Tags = ResizeArray tags)
                        ]
                    )

                let! imageResult = 
                    ec2Client.CreateImageAsync(imageRequest)
                    |> Async.AwaitTask
                    >>>= fun response -> waitForImage ec2Client response.ImageId

                return 
                    match imageResult with
                    | Ok _ -> Ok amiRequest.Instance
                    | Error err -> Error err
        }

    /// Start an EC2 instance
    let startInstance (ec2Client: AmazonEC2Client) (instance: Instance) =
        async {
            do! ec2Client.StartInstancesAsync(StartInstancesRequest(InstanceIds = ResizeArray [ instance.InstanceId ]))
                |> Async.AwaitTask
                |> Async.Ignore

            let rec waitUntilStarted() = async {
                let! instanceResult = getInstanceById ec2Client instance.InstanceId

                match instanceResult with
                | Ok startedInstance when startedInstance.State.Name = InstanceStateName.Running -> 
                    return Ok startedInstance
                | Ok _ -> 
                    do! Async.Sleep 2500
                    return! waitUntilStarted()
                | Error msg -> 
                    return Error(OperationError.ErrorStartingInstance 
                        $"Error when starting instance '{Helpers.displayName instance}': {msg}")
            }

            return! waitUntilStarted()
        }