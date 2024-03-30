module UnityData

open UnityDataTools.FileSystem

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open MicroUtils.UnityFilesystem
open MicroUtils.UnityFilesystem.Parsers
open MicroUtils.UnityFilesystem.Converters

let mountPoint = @"archive:/"

let getDependenciesAsync dependencylistJson = async {
    use stream = System.IO.File.OpenRead dependencylistJson
    use textReader = new System.IO.StreamReader(stream)
    use jReader = new JsonTextReader(textReader)
    
    let! json = JObject.LoadAsync(jReader, Async.DefaultCancellationToken) |> Async.AwaitTask
    
    let jp =
        json.Property("BundleToDependencies")
        |> Option.ofObj
        |> Option.orElseWith (fun () -> json.Property("m_List") |> Option.ofObj)

    let wotr (arr : JArray) =    
        arr
        |> Seq.choose (function
        | :? JObject as entry ->
            let key = 
                entry.Properties()
                |> Seq.tryFind (fun p -> p.Name = "Key")
                |> Option.map (fun key -> key.Value.ToObject<string>())
            
            let value =
                entry.Properties()
                |> Seq.tryFind (fun p -> p.Name = "Value")
                |> Option.bind (fun p ->
                    match p.Value with
                    | :? JArray as deps -> deps |> Seq.map (fun d -> d.ToObject<string>()) |> Seq.toArray |> Some
                    | _ -> None)

            match key, value with
            | Some key, Some value -> Some (key, value)
            | _ -> None

        | _ -> None)

    let rt (jObject : JObject) =
        jObject.Properties()
        |> Seq.map (fun p -> p.Name, p.Value)
        |> Seq.choose (function
        | _, :? JArray as (name, dependencies) ->
            let deps = 
                dependencies
                |> Seq.map (fun d -> d.ToObject<string>())
                |> Seq.toArray
            Some (name, deps)
        | _ -> None)

    let dependencies =
        jp
        |> Option.bind (fun jp ->
            match jp.Value with
            | :? JObject as obj -> rt obj |> Some
            | :? JArray as arr -> wotr arr |> Some
            | _ -> None
        )
        |> Option.toArray
        |> Seq.collect id
        |> Map.ofSeq

    if Map.isEmpty dependencies then
        eprintfn $"Could not load dependency data from {dependencylistJson}"
        
    return dependencies
}

let getDependencies = getDependenciesAsync >> Async.RunSynchronously

let toPixelRect (rect : Rectf) =
    Avalonia.PixelRect(Avalonia.PixelPoint(rect.x |> int, rect.y |> int), Avalonia.PixelSize(rect.width |> int, rect.height |> int))

let getReader (readers : (string * UnityBinaryFileReader) list) path =
    readers
    |> Seq.tryFind (fun (readerPath, _) -> readerPath = path)
    |> function
    | Some (_, r) -> r, readers
    | None ->
        let reader = new UnityBinaryFileReader(path)
        reader, ((path, reader) :: readers)

// let flipTextureY (input : byte[]) width =
//     let output = Array.zeroCreate<byte> input.Length

//     for i in 0..((input.Length / width) - 1) do
//         let iStart = i * width
//         let oStart = (output.Length - ((i + 1) * width))

//         System.Buffer.BlockCopy(input, iStart, output, oStart, width)

//     output

type MicroUtils.UnityFilesystem.ITypeTreeObject with
    member this.TryGetName() =
        match this.ToDictionary().TryGetValue("m_Name") with
        | true, name ->
            Some (name.GetValue<string>()) 
        | _ -> None

let fileIDToPath fid (sf : SerializedFile) =
    if fid = 0 then Some sf.Path
    elif fid < sf.ExternalReferences.Count - 1 then
        let refPath = sf.ExternalReferences[fid - 1].Path |> UnityReferencePath
        refPath.ToFilePath() |> Some
    else
        None

let getSerializedFilePaths (archive : UnityArchive) =
    archive.Nodes
    |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
    |> Seq.map (fun n -> $"{mountPoint}{n.Path}" )