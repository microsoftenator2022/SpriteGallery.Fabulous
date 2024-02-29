namespace SpriteGallery.Fabulous

open Avalonia.Platform
open Avalonia.Media.Imaging
open Avalonia.Themes.Fluent

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open UnityDataTools.FileSystem

open MicroUtils.UnityFilesystem
open MicroUtils.UnityFilesystem.Parsers
open MicroUtils.UnityFilesystem.Converters

open SpriteGallery.Fabulous.MicroUtils

type Sprite =
  { Bitmap : Bitmap
    TextureRect : Avalonia.PixelRect }

[<RequireQualifiedAccess>]
module Sprites =
    open UnityData

    let get (archives : UnityArchive[]) path =
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
                        s.RenderDataKey
                        |> toOption
                        |> Option.bind (fun rdk ->
                            let succ, sad = atlas.RenderDataMap.TryGetValue(rdk)

                            if succ then Some sad else None)
                        |> Option.bind (fun sad ->
                            let bitmap = sad.Texture |> getBitmap
                            let rect = sad.TextureRect
                            bitmap
                            |> Option.map (fun bitmap -> bitmap, rect))
                        |> Option.map (fun (bitmap, rect) -> { Bitmap = bitmap; TextureRect = rect |> toPixelRect })
                    | None ->
                        s.TexturePtr
                        |> toOption
                        |> Option.bind getBitmap
                        |> Option.map (fun bitmap ->
                              { Bitmap = bitmap;
                                TextureRect =
                                    s.Rect
                                    |> toOption
                                    |> function
                                    | Some rect -> rect |> toPixelRect
                                    | None -> Avalonia.PixelRect(Avalonia.PixelPoint(0, 0), bitmap.PixelSize) })
                sprite, name
            )
            |> Seq.toArray

        for (_, reader) in readers do
            reader.Dispose()

        for sf in serializedFiles do
            sf.Dispose()

        bitmaps, sprites

type Model =
    {
        Bitmaps : Map<(int * int64), Bitmap>
        Sprites : (Bitmap * Avalonia.PixelRect * string) list
    }

module Model =
    let init (archiveFile : string) =
        // let archiveFile = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"

        UnityFileSystem.Init()

        let bundlesDirectory = System.IO.Path.GetDirectoryName(archiveFile)

        let dependencylist =
            System.IO.Path.Join(bundlesDirectory, "dependencylist.json")

        let dependencies =
            if System.IO.File.Exists dependencylist then
                UnityData.getDependencies dependencylist
                |> Map.tryFind (System.IO.Path.GetFileName(archiveFile))
                |> Option.map (fun files ->
                    seq {
                        for f in files do
                            let path = System.IO.Path.Join(bundlesDirectory, f)
                            UnityFileSystem.MountArchive(path, UnityData.mountPoint)
                    }
                    |> Seq.toArray)
                |> function
                | Some list -> list
                | None -> [||]
            else [||]

        let archive = UnityFileSystem.MountArchive(archiveFile, UnityData.mountPoint)

        let sfNode =
            archive.Nodes
            |> Seq.find (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))

        let sfPath = $"{UnityData.mountPoint}{sfNode.Path}"

        let bitmaps, sprites = Sprites.get dependencies sfPath

        let sprites =
            sprites
            |> Seq.choose (
                function
                | Some sprite, name ->
                    Some (sprite.Bitmap, sprite.TextureRect, name |> Option.defaultValue "" |> sprintf "%A")
                | _ -> None
            )

        archive.Dispose()
        
        for archive in dependencies do
            archive.Dispose()

        UnityFileSystem.Cleanup()

        { Bitmaps = bitmaps; Sprites = sprites |> Seq.sortBy (fun (_, _, name) -> name) |> Seq.toList }

type Msg = unit
