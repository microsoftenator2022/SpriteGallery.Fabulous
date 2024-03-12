namespace SpriteGallery.Fabulous
#nowarn "3391"

open Fabulous.Avalonia
open Fabulous

open UnityDataTools.FileSystem

open MicroUtils
open MicroUtils.UnityFilesystem
open MicroUtils.UnityFilesystem.Parsers
open MicroUtils.UnityFilesystem.Converters

open UnityData

open MicroUtils.Interop

type MicroOption<'a> = MicroUtils.Functional.Option<'a>

type SpriteTexture (bytes : byte[], size : Avalonia.PixelSize) =
    let createBitmap() =
        new Avalonia.Media.Imaging.Bitmap(
            Avalonia.Platform.PixelFormat.Bgra8888,
            Avalonia.Platform.AlphaFormat.Unpremul,
            System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(bytes, 0),
            size,
            Avalonia.Vector(96, 96),
            size.Width * 4)

    member val Bitmap = lazy (createBitmap())
    member _.Bytes = bytes
    member _.Size = size

    interface System.IDisposable with
        member this.Dispose() =
            if this.Bitmap.IsValueCreated then
                this.Bitmap.Value.Dispose()
    
type Sprite =
  { Texture : SpriteTexture
    Rect : Avalonia.PixelRect }

[<RequireQualifiedAccess>]
module Sprites =
    let get (updateProgress : (int * int) -> unit) (archives : UnityArchive[]) path =
        let sfNodes =
            archives
            |> Seq.collect (fun archive -> archive.Nodes)
            |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
            |> Seq.cache

        use sf = UnityFileSystem.OpenSerializedFile path
        let sfReader = new UnityBinaryFileReader(sf.Path)

        let mutable readers = [(sf.Path, sfReader)]

        let getReader path : MicroOption<UnityBinaryFileReader> =
            let r, rs = getReader readers path
            readers <- rs
            Some r |> MicroOption.op_Implicit

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
            |> MicroOption.op_Implicit

        let decodeTexture (texture : Texture2D) =
            let buffer = Array.zeroCreate(4 * texture.Width * texture.Height)
            if Texture2DConverter.DecodeTexture2D(texture, System.Span(buffer), getReader) then
                Some buffer
            else None
            
        let mutable textures : Map<(int * int64), SpriteTexture> = Map.empty
        
        let getTexture (pptr : PPtr) =
            let key = (pptr.FileID, pptr.PathID)
            textures
            |> Map.tryFind key
            |> Option.orElseWith (fun () ->

                #if DEBUG
                printfn $"Load texture PPtr: {pptr.SerializedFilePath} -> FileID = {pptr.FileID}, PathID = {pptr.PathID}"
                #endif

                let opt : Option<ITypeTreeValue> = pptr.TryDereference(getSerializedFile, getReader)
                opt
                |> Option.bind (
                    function
                    | :? Texture2D as texture -> 
                        decodeTexture texture
                        |> Option.map (fun bytes -> new SpriteTexture(bytes, Avalonia.PixelSize(texture.Width, texture.Height)))
                    | _ -> None))
                |> Option.map (fun texture ->
                    textures <- textures |> Map.add key texture
                    texture)

        let spriteObjects =
            sf.Objects
            |> Seq.map (fun o -> (o, sf.GetTypeTreeRoot(o.Id)))
            |> Seq.where (fun (_, tt) -> tt.Type = "Sprite")
            |> Seq.toArray

        let total = spriteObjects.Length
        let mutable i = 0

        let sprites =
            spriteObjects
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
                    |> MicroOption.op_Implicit
                    |> function
                    | Some ap when ap <> PPtr.NullPtr ->
                        ap.TryDereference(getSerializedFile, getReader)
                        |> toOption
                        |> Option.bind (function :? Parsers.SpriteAtlas as sa -> Some sa | _ -> None)
                    | _ -> None

                #if DEBUG
                name
                |> Option.defaultValue ""
                |> printfn "Load sprite \"%s\""
                #endif
                    
                let sprite =
                    match atlas with
                    | Some atlas ->
                        s.RenderDataKey
                        |> toOption
                        |> Option.bind (fun rdk ->
                            let succ, sad = atlas.RenderDataMap.TryGetValue(rdk)

                            if succ then Some sad else None)
                        |> Option.bind (fun sad ->
                            let texture = sad.Texture |> getTexture
                            let rect = sad.TextureRect
                            texture
                            |> Option.map (fun texture -> texture, rect))
                        |> Option.map (fun (texture, rect) -> { Texture = texture; Rect = rect |> toPixelRect })
                    | None ->
                        s.TexturePtr
                        |> toOption
                        |> Option.bind getTexture
                        |> Option.map (fun texture ->
                            let rect = 
                                s.Rect
                                |> toOption
                                |> function
                                | Some rect -> rect |> toPixelRect
                                | None -> Avalonia.PixelRect(Avalonia.PixelPoint(0, 0), texture.Size)
                            
                            { Texture = texture; Rect = rect }
                        )
                i <- i + 1

                updateProgress (i, total)

                sprite, name
            )
            |> Seq.toArray

        for (_, reader) in readers do
            reader.Dispose()

        for sf in serializedFiles do
            sf.Dispose()

        printfn "Got %i sprites, %i textures" (sprites.Length) (textures |> Map.count)

        textures, sprites

type SpriteGetter (archiveFile : string, ?includeDependencies : bool) =
    let mountArchive file =
        printfn "Mounting archive %s" file
        UnityFileSystem.MountArchive(file, UnityData.mountPoint)

    let includeDependencies = defaultArg includeDependencies true
    let mutable progress : int * int = (0, 1)
    let update = Event<int * int>()

    let mutable result = None

    let newWaitHandle() = lazy (new System.Threading.EventWaitHandle(false, System.Threading.EventResetMode.AutoReset))
    let mutable waitHandle = newWaitHandle()

    let get() =
        UnityFileSystem.Init()

        let bundlesDirectory = System.IO.Path.GetDirectoryName(archiveFile)

        let dependencylist =
            System.IO.Path.Join(bundlesDirectory, "dependencylist.json")

        let dependencies =
            if includeDependencies && System.IO.File.Exists dependencylist then
                UnityData.getDependencies dependencylist
                |> Map.tryFind (System.IO.Path.GetFileName(archiveFile))
                |> Option.map (fun files ->
                    seq {
                        for f in files do
                            let path = System.IO.Path.Join(bundlesDirectory, f)
                            mountArchive path
                    }
                    |> Seq.toList)
                |> function
                | Some list -> list
                | None -> []
            else []

        let archive = mountArchive archiveFile

        let sfNode =
            archive.Nodes
            |> Seq.find (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))

        let sfPath = $"{UnityData.mountPoint}{sfNode.Path}"

        let updateProgress (current, total) =
        #if DEBUG
            ()
        #else
            if current % (total / 100) = 0 then
                printfn "Loading sprites: %i of %i" current total
        #endif
            progress <- (current, total)

            update.Trigger(progress)

        let textures, sprites = Sprites.get updateProgress (archive :: dependencies |> List.toArray) sfPath

        let sprites =
            sprites
            |> Seq.choose (
                function
                | Some sprite, name ->
                    Some (sprite, name |> Option.defaultValue "" |> sprintf "%A")
                | _ -> None
            )

        archive.Dispose()
        
        for archive in dependencies do
            archive.Dispose()

        UnityFileSystem.Cleanup()

        textures, sprites

    [<CLIEvent>]
    member _.Update = update.Publish

    member _.Progress = progress
    member _.Complete =
        // let (current, total) = progress

        // total >= 0 && current = total
        result.IsSome

    member _.Start() =
        let thread =
            System.Threading.ThreadStart (fun () ->
                result <- get() |> Some
                update.Trigger progress
                waitHandle.Force().Set() |> ignore)
            |> System.Threading.Thread
        
        thread.IsBackground <- true

        thread.Start()

    member _.Textures = result |> Option.map (fun (ts, _) -> ts)
    member _.Sprites = result |> Option.map (fun (_, ss) -> ss)

    member this.GetAsync (?timeout) = async {
        let timeout = defaultArg timeout -1
        this.Start()
        return! Async.AwaitWaitHandle(waitHandle.Force(), timeout)
    }

    interface System.IDisposable with
        member _.Dispose() =
            if waitHandle.IsValueCreated then
                waitHandle.Value.Dispose()
                waitHandle <- newWaitHandle()

// module BundleLoadView =
//     open type Fabulous.Avalonia.View

//     type LoadProgress = { Complete : bool; Progress : int * int }

//     [<RequireQualifiedAccess>]
//     module LoadProgress =
//         type Msg =
//         | Unit
//         | Update of LoadProgress

//         type Model = LoadProgress

//         // let update (msg : Msg) (model : BaseModel * Model)

//         let view (model : LoadProgress) =
//             let (current, total) = model.Progress
//             (Dock(true) {
//                 Button("Open...", Unit)
//                     .margin(8)

//                 ProgressBar(0, total, current, fun _ -> Unit)
//                     .showProgressText(true)
//                     .margin(8)
//             })
//                 .verticalAlignment(Avalonia.Layout.VerticalAlignment.Top)
            
    