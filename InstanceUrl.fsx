open System
open System.IO

let toUrls instanceNames =
    let formattedInstances = instanceNames |> Seq.reduce (sprintf "%s,%s")

    [ for region in [ "east"; "west" ] ->
        $"https://console.amazonaws-us-gov.com/ec2/home?region=us-gov-{region}-1#Instances:tag:Name={formattedInstances};v=3;$case=tags:true%%5C,client:false;$regex=tags:false%%5C,client:false"
        ]

File.ReadAllLines "instances.txt"
|> Seq.map _.Trim()
|> Seq.filter (String.IsNullOrWhiteSpace >> not)
|> toUrls
|> Seq.iter (printfn "%s")