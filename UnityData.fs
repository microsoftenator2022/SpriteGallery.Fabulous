module UnityData

open UnityDataTools.FileSystem

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open MicroUtils.UnityFilesystem
open MicroUtils.UnityFilesystem.Parsers
open MicroUtils.UnityFilesystem.Converters

open SpriteGallery.Fabulous.MicroUtils

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

type Sprite =
  { Bitmap : Avalonia.Media.Imaging.Bitmap
    TextureRect : Avalonia.PixelRect option }

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

let getSprites (archives : UnityArchive[]) path =
    let sfNodes =
        archives
        |> Seq.collect (fun archive -> archive.Nodes)
        |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
        |> Seq.cache

    use sf = UnityFileSystem.OpenSerializedFile path
    let sfReader = new UnityBinaryFileReader(sf.Path)

    let mutable readers = [(sf.Path, sfReader)]

    let getReader path =
        let r, rs = getReader readers path
        readers <- rs
        r |> Some |> toMicroOption

    let mutable serializedFiles : SerializedFile list = []

    let getSerializedFile path =
        if path = sf.Path then
            Some sf
        else
            serializedFiles
            |> Seq.tryFind (fun sf -> sf.Path = path)
            |> Option.orElseWith (fun () ->
                if sfNodes |> Seq.exists (fun n -> $"{mountPoint}{n.Path}" = path) then
                    let sf = UnityFileSystem.OpenSerializedFile(path)
                    serializedFiles <- sf :: serializedFiles
                    Some sf
                else None)
        |> toMicroOption

    let createBitmap (texture : Texture2D) =
        let buffer = Array.zeroCreate(4 * texture.Width * texture.Height)
        if Texture2DConverter.DecodeTexture2D(texture, System.Span(buffer), getReader) then
            // let buffer = flipTextureY buffer (texture.Width * 4)
            
            new Avalonia.Media.Imaging.Bitmap(
                Avalonia.Platform.PixelFormat.Bgra8888,
                Avalonia.Platform.AlphaFormat.Unpremul,
                System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(buffer, 0),
                Avalonia.PixelSize(texture.Width, texture.Height),
                Avalonia.Vector(96, 96),
                texture.Width * 4)
            |> Some
        else None

    let mutable bitmaps : Map<(int * int64), Avalonia.Media.Imaging.Bitmap> = Map.empty
    
    let getBitmap (pptr : PPtr) =
        let key = (pptr.FileID, pptr.PathID)
        bitmaps
        |> Map.tryFind key
        |> Option.orElseWith (fun () ->
            pptr.TryDereference(getSerializedFile, getReader)
            |> toOption
            |> Option.bind (
                function
                | :? Texture2D as texture -> createBitmap texture
                | _ -> None))
            |> Option.map (fun bitmap ->
                bitmaps <- bitmaps |> Map.add key bitmap
                bitmap)

    let sprites =
        sf.Objects
        |> Seq.map (fun o -> (o, sf.GetTypeTreeRoot(o.Id)))
        |> Seq.where (fun (_, tt) -> tt.Type = "Sprite")
        |> Seq.choose (fun (o, _) ->
            TypeTreeValue.Get(sf, sfReader, o)
            |> function
            | :? Parsers.Sprite as s -> Some s
            | _ -> None)
        |> Seq.map (fun s ->
            let name =
                let succ, name = s.ToDictionary().TryGetValue("m_Name")
                if succ then Some (name.GetValue<string>()) else None

            let atlas =
                s.AtlasPtr
                |> toOption
                |> function
                | Some ap when ap <> PPtr.NullPtr ->
                    ap.TryDereference(getSerializedFile, getReader)
                    |> toOption
                    |> Option.bind (function :? Parsers.SpriteAtlas as sa -> Some sa | _ -> None)
                | _ -> None
                
            let sprite =
                match atlas with
                | Some atlas ->
                    let sad =
                        s.RenderDataKey
                        |> toOption
                        |> Option.bind (fun rdk ->
                            let succ, sad = atlas.RenderDataMap.TryGetValue(rdk)

                            if succ then Some sad else None)

                    sad
                    |> Option.bind (fun sad ->
                        let bitmap = sad.Texture |> getBitmap
                        let rect = sad.TextureRect
                        bitmap
                        |> Option.map (fun bitmap -> bitmap, rect))
                    |> Option.map (fun (bitmap, rect) -> { Bitmap = bitmap; TextureRect = rect |> toPixelRect |> Some })
                | None ->
                    s.TexturePtr
                    |> toOption
                    |> Option.bind getBitmap
                    |> Option.map (fun bitmap ->
                        { Bitmap = bitmap; TextureRect = s.Rect |> toOption |> Option.map toPixelRect })
            sprite, name
        )
        |> Seq.toArray

    for (_, reader) in readers do
        reader.Dispose()

    sprites
