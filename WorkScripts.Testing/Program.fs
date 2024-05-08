open k8s
open k8s.Models

let config = KubernetesClientConfiguration.BuildConfigFromConfigFile()
let client = new Kubernetes(config)

let deployments = client.ListDeploymentForAllNamespaces()
let pods = client.ListPodForAllNamespaces()

printfn "%s" (config.CurrentContext)

let printInformation (pod: V1Pod) = 
    let information = {|
        replicas = ()
    |}

    printfn "%A" information

pods.Items
|> Seq.iter printInformation
