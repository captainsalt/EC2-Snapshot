module SnapshotArgs

open Argu
open System

type Arguments =
    | Regions of regions: string list
    | Profile of profile_name: string
    | [<Mandatory; AltCommandLine("-f")>] Input of file_path: string
    | [<Mandatory>] Task of string
    | [<Mandatory; AltCommandLine("-d")>] Description of string
    | [<AltCommandLine("--stop")>] Stop_Instances
    | [<AltCommandLine("--start")>] Start_Instances
    | Ignore_Errors

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Regions _ -> "List of regions to search for instances"
            | Input _ -> "Path to a file with a list of instance ids"
            | Task _ -> "The SNOW ticket for this snapshot request. Example: 'CTASK0088254'"
            | Description _ ->
                $"""Description of the snapshot resource. Example: '{DateTime.Now.ToString("M/d/yyyy")} CTASK0088254'"""
            | Stop_Instances -> "Use this flag if you want the script to stop the instances before taking snapshots"
            | Start_Instances ->
                "Use this flag if you want the script to start the instances after the snapshots are done"
            | Profile _ -> "Use this flag to specify an aws profile to use"
            | Ignore_Errors -> "If the script can't find any of the instances specified in the input file, it will stop. Use this flag to bypass this behaviour"

let cliParser = ArgumentParser.Create<Arguments>()
