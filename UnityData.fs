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
    
    let dependencies =
        json.Property("BundleToDependencies")
        |> Option.ofObj
        |> Option.bind (fun p -> 
            match p.Value with
            | :? JObject as o -> Some o
            | _ -> None)
        |> function
        | Some bundles ->
            bundles.Properties()
            |> Seq.map (fun p -> p.Name, p.Value)
            |> Seq.choose (function
            | _, :? JArray as (name, dependencies) ->
                let deps = 
                    dependencies
                    |> Seq.map (fun d -> d.ToObject<string>())
                    |> Seq.toArray
                Some (name, deps)
            | _ -> None)
        | None -> Seq.empty
        |> Map.ofSeq
        
    return dependencies
}

let getDependencies = getDependenciesAsync >> Async.RunSynchronously

// type SpriteAtlas =
//   { Bitmaps : Avalonia.Media.Imaging.Bitmap list
//     TextureRectMap : Map<(System.Guid * int64), Avalonia.PixelRect> }

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
