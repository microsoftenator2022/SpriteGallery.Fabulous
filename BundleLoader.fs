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

open SpriteGallery.Fabulous.Common

type MicroOption<'a> = MicroUtils.Functional.Option<'a>

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

        let assetBundleAsset =
            sf.Objects
            |> Seq.tryFind (fun o -> sf.GetTypeTreeRoot(o.Id).Type = "AssetBundle")
            |> Option.bind (fun o -> TypeTreeValue.Get(sf, sfReader, o).TryGetValue<AssetBundle>() |> toOption)
            |> Option.map (fun f -> f.Invoke())

        let containerMap = 
            assetBundleAsset
            |> Option.toArray
            |> Seq.collect (fun ab ->
                ab.ContainerMap
                |> Seq.collect (fun cm -> cm.Value |> Seq.map (fun ai -> ai, cm.Key))
                |> Seq.collect (fun (ai, cid) ->
                    ai.GetAllAssetPPtrs(ab.PreloadTable)
                    |> Seq.append [ai.Asset]
                    |> Seq.map (fun pptr -> pptr.PathID)
                    |> Seq.distinct
                    |> Seq.map (fun pid -> pid, cid)))
            |> Map.ofSeq

        let blueprintReferencedAssets =
            sf.Objects
            |> Seq.where (fun o ->
                let tt = sf.GetTypeTreeRoot(o.Id)
                tt.Type = "MonoBehaviour"
                && tt.Children |> Seq.exists (fun c -> c.Name = "m_Entries" && c.Type = "Entry"))
            |> Seq.choose (fun o -> TypeTreeValue.Get(sf, sfReader, o).TryGetObject() |> toOption)
            |> Seq.tryFind (fun o -> o |> TypeTreeObject.tryGetField "m_Name" = Some "BlueprintReferencedAssets")
            |> Option.bind (TypeTreeObject.toMap >> Map.tryFind "m_Entries")
            |> Option.bind (fun o -> o.TryGetObject() |> toOption)
            |> Option.bind (TypeTreeObject.toMap >> Map.tryFind "Array")
            |> Option.bind (fun o -> o.TryGetArray<ITypeTreeObject>() |> toOption)
            |> Option.toArray
            |> Seq.collect id
            |> Seq.choose (fun o ->
                let assetId = o |> TypeTreeObject.tryGetField<string> "AssetId"
                let fileId = o |> TypeTreeObject.tryGetField<int64> "FileId"
                let asset = o |> TypeTreeObject.tryGetField<PPtr> "Asset"

                match assetId, fileId, asset with
                | Some assetId, Some fileId, Some asset -> Some (asset.PathID, (assetId, fileId))
                | _ -> None)
            |> Map.ofSeq

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

        let mutable pptrCache : Map<int64, ITypeTreeValue> = Map.empty

        let dereference (pptr : PPtr) =
            pptrCache
            |> Map.tryFind pptr.PathID
            |> Option.orElseWith (fun () ->
                pptr.TryDereference(getSerializedFile, getReader)
                |> toOption
                |> Option.bind (fun v -> pptrCache <- pptrCache |> Map.add pptr.PathID v; Some v))

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

                let opt : Option<ITypeTreeValue> = dereference pptr
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
                | :? Parsers.Sprite as s -> Some (s, o)
                | _ -> None)
            |> Seq.map (fun (s, o) ->
                let name =
                    match s.ToDictionary().TryGetValue("m_Name") with
                    | true, name ->
                        Some (name.GetValue<string>()) 
                    | _ -> None

                let atlas =
                    s.AtlasPtr
                    |> MicroOption.op_Implicit
                    |> function
                    | Some ap when ap <> PPtr.NullPtr ->
                        dereference ap
                        |> Option.bind (function :? Parsers.SpriteAtlas as sa -> Some sa | _ -> None)
                    | _ -> None

                #if DEBUG
                name
                |> Option.defaultValue ""
                |> printfn "Load sprite \"%s\""
                #endif

                let renderDataKey = s.RenderDataKey |> toOption

                let sprite =
                    match atlas with
                    | Some atlas ->
                        renderDataKey
                        |> Option.bind (fun rdk ->
                            let succ, sad = atlas.RenderDataMap.TryGetValue(rdk)

                            if succ then Some sad else None)
                        |> Option.bind (fun sad ->
                            let texture = sad.Texture |> getTexture
                            let rect = sad.TextureRect
                            texture
                            |> Option.map (fun texture -> texture, rect))
                        |> Option.map (fun (texture, rect) -> Sprite.Create(texture, rect |> toPixelRect))
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
                            
                            Sprite.Create(texture, rect)
                        )
                i <- i + 1

                updateProgress (i, total)
                sprite
                |> Option.map (fun sprite ->
                    { sprite with
                        Name = name
                        RenderDataKey = renderDataKey
                        SerializedFile = sf.Path
                        PathID = o.Id
                        Container =
                            containerMap
                            |> Map.tryFind o.Id
                            |> Option.defaultValue ""
                        BlueprintReference =
                            blueprintReferencedAssets
                            |> Map.tryFind o.Id
                    })
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

        let mutable archives = [||]

        let mountArchive file =
            archives
            |> Seq.tryFind (fun (path, _) -> path = file)
            |> Option.map snd
            |> Option.defaultWith (fun () ->

                printfn "Mounting archive %s" file

                let archive = UnityFileSystem.MountArchive(file, UnityData.mountPoint)

                archives <-
                    archives |> Array.appendOne (file, archive)

                archive
            )

        let getDependenciesMapAsync (dir : string) = async {
            let dependencyJsonFile = System.IO.Path.Join(dir, "dependencylist.json")

            return! UnityData.getDependenciesAsync dependencyJsonFile
        }

        let getRecursiveArchiveDependencies (archiveFileName : string) (dependenciesMap : Map<string, string[]>) =
            let mutable dependencies = [archiveFileName]

            let rec getDeps f =
                dependenciesMap
                |> Map.tryFind f
                |> Option.iter (fun ds ->
                    for d in ds do
                        if dependencies |> Seq.contains d |> not then
                            dependencies <- d :: dependencies
                            getDeps d
                )

            getDeps archiveFileName

            dependencies

        let mountArchiveWithDependencies (archiveFile : string) =
            let dir = System.IO.Path.GetDirectoryName(archiveFile)
            let fileName = System.IO.Path.GetFileName(archiveFile)

            let dependenciesMap =
                getDependenciesMapAsync dir
                |> Async.RunSynchronously
                
            let dependenciesPaths =
                getRecursiveArchiveDependencies fileName dependenciesMap
                |> Seq.map (fun name -> System.IO.Path.Join(dir, name))

            seq {
                yield!
                    dependenciesPaths
                    |> Seq.map (fun path -> mountArchive path)
                
                yield mountArchive archiveFile
            }
            |> Seq.toArray

        // let bundlesDirectory = System.IO.Path.GetDirectoryName(archiveFile)

        // let dependencylist = System.IO.Path.Join(bundlesDirectory, "dependencylist.json")

        // let dependencies =
        //     if includeDependencies && System.IO.File.Exists dependencylist then
        //         UnityData.getDependencies dependencylist
        //         |> Map.tryFind (System.IO.Path.GetFileName(archiveFile))
        //         |> Option.map (fun files ->
        //             seq {
        //                 for f in files do
        //                     let path = System.IO.Path.Join(bundlesDirectory, f)
        //                     mountArchive path
        //             }
        //             |> Seq.toList)
        //         |> function
        //         | Some list -> list
        //         | None -> []
        //     else []

        mountArchiveWithDependencies archiveFile |> ignore

        let archive = mountArchive archiveFile

        let sfNodes =
            archive.Nodes
            |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))

        let sfPaths = sfNodes |> Seq.map (fun sfNode -> $"{UnityData.mountPoint}{sfNode.Path}")

        let updateProgress (current, total) =
        #if DEBUG
            ()
        #else
            if current % (total / 100) = 0 then
                printfn "Loading sprites: %i of %i" current total
        #endif
            progress <- (current, total)

            update.Trigger(progress)

        let results =
            sfPaths
            |> Seq.map (fun sfPath -> Sprites.get updateProgress (archives |> Array.map snd) sfPath)
            |> Seq.toArray

        let textures =
            results
            |> Seq.collect (fun (textures, _) -> textures)
            |> Seq.map (fun t -> t.Key, t.Value)
            |> Map.ofSeq

        let sprites =
            results
            |> Seq.collect(fun (_, sprites) -> sprites)
            |> Seq.choose id
            |> Seq.toArray

        archive.Dispose()
        
        for (_, archive) in archives do
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

    override this.Finalize() = (this :> System.IDisposable).Dispose()

type BlueprintAssetReference = { AssetID : string; FileID : int64 }

[<RequireQualifiedAccessAttribute>]
module AssetLoader =
    let private initReaderCache() = new System.Threading.ThreadLocal<(string * UnityBinaryFileReader)[]>((fun () -> [||]), true)
    let mutable private readers = initReaderCache()
    
    let initSerializedFileCache() = new System.Threading.ThreadLocal<SerializedFile[]>((fun () -> [||]), true)
    let mutable private serializedFiles = initSerializedFileCache()

    let initArchiveCache() = new System.Threading.ThreadLocal<(string * UnityArchive)[]>((fun () -> [||]), true)
    let mutable private archives = initArchiveCache()

    let mutable private containersMap = Map.empty

    let mutable private blueprintReferencedAssets : Map<(string * int64), BlueprintAssetReference> = Map.empty

    let mutable private pptrCache : Map<(string * int64), ITypeTreeValue> = Map.empty
    
    let init = UnityFileSystem.Init

    let cleanup() =
        containersMap <- Map.empty
        blueprintReferencedAssets <- Map.empty
        pptrCache <- Map.empty

        for (_, r) in readers.Values |> Seq.collect id do
            r.Dispose()

        readers.Dispose()
        readers <- initReaderCache()
        
        for sf in serializedFiles.Values |> Seq.collect id do
            sf.Dispose()

        serializedFiles.Dispose()
        serializedFiles <- initSerializedFileCache()

        for (_, archive) in archives.Values |> Seq.collect id do
            archive.Dispose()

        archives.Dispose()
        archives <-initArchiveCache()

        UnityFileSystem.Cleanup()

    let mountArchive file =
        archives.Value
        |> Seq.tryFind (fun (path, _) -> path = file)
        |> Option.map snd
        |> Option.defaultWith (fun () ->

            printfn "Mounting archive %s" file

            let archive = UnityFileSystem.MountArchive(file, UnityData.mountPoint)

            archives.Value <-
                archives.Value |> Array.appendOne (file, archive)

            archive
        )

    let getDependenciesMapAsync (dir : string) = async {
        let dependencyJsonFile = System.IO.Path.Join(dir, "dependencylist.json")

        return! UnityData.getDependenciesAsync dependencyJsonFile
    }

    let getRecursiveArchiveDependencies (archiveFileName : string) (dependenciesMap : Map<string, string[]>) =
        let mutable dependencies = [archiveFileName]

        let rec getDeps f =
            dependenciesMap
            |> Map.tryFind f
            |> Option.iter (fun ds ->
                for d in ds do
                    if dependencies |> Seq.contains d |> not then
                        dependencies <- d :: dependencies
                        getDeps d
            )

        getDeps archiveFileName

        dependencies

    let mountArchiveWithDependencies (archiveFile : string) =
        let dir = System.IO.Path.GetDirectoryName(archiveFile)
        let fileName = System.IO.Path.GetFileName(archiveFile)

        let dependenciesMap =
            getDependenciesMapAsync dir
            |> Async.RunSynchronously

        let dependenciesPaths =
            getRecursiveArchiveDependencies fileName dependenciesMap
            |> Seq.map (fun name -> System.IO.Path.Join(dir, name))

        seq {
            yield!
                dependenciesPaths
                |> Seq.map (fun path -> mountArchive path)
            
            yield mountArchive archiveFile
        }
        |> Seq.toArray

    let getReader path =
        let r, rs = getReader (readers.Value |> Array.toList) path
        readers.Value <- rs |> List.toArray
        r

    let getSerializedFile path =
        let sfPaths =
            archives.Value 
            |> Seq.collect (snd >> getSerializedFilePaths)

        serializedFiles.Value
        |> Seq.tryFind (fun sf -> sf.Path = path)
        |> Option.orElseWith (fun () ->
            if sfPaths |> Seq.contains path then
                let sf = UnityFileSystem.OpenSerializedFile(path)
                serializedFiles.Value <- sf :: (serializedFiles.Value |> Array.toList) |> List.toArray
                Some sf
            else None)
        |> MicroOption.op_Implicit

    let dereference (pptr : PPtr) =
        pptrCache
        |> Map.tryFind (pptr.SerializedFilePath, pptr.PathID)
        |> Option.orElseWith (fun () ->
            pptr.TryDereference(getSerializedFile, getReader >> MicroOption.Some)
            |> toOption
            |> Option.bind (fun v -> pptrCache <- pptrCache |> Map.add (pptr.SerializedFilePath, pptr.PathID) v; Some v))

    let getAssetBundleAsset (sf : SerializedFile) =
        let sfReader = getReader sf.Path
        
        sf.Objects
        |> Seq.tryFind (fun o -> sf.GetTypeTreeRoot(o.Id).Type = "AssetBundle")
        |> Option.bind (fun o -> TypeTreeValue.Get(sf, sfReader, o).TryGetValue<AssetBundle>() |> toOption)
        |> Option.map (fun f -> f.Invoke())

    let getContainerMap sf =
        getAssetBundleAsset sf
        |> Option.toArray
        |> Seq.collect (fun ab ->
            ab.ContainerMap
            |> Seq.collect (fun cm -> cm.Value |> Seq.map (fun ai -> ai, cm.Key))
            |> Seq.collect (fun (ai, cid) ->
                ai.GetAllAssetPPtrs(ab.PreloadTable)
                |> Seq.append [ai.Asset]
                |> Seq.distinctBy (fun pptr -> pptr.PathID)
                |> Seq.map (fun pptr -> pptr, cid)
            )
        )

    let getBlueprintReferencedAssets (sf : SerializedFile) =
        let sfReader = getReader sf.Path

        sf.Objects
        |> Seq.where (fun o ->
            let tt = sf.GetTypeTreeRoot(o.Id)
            tt.Type = "MonoBehaviour"
            && tt.Children |> Seq.exists (fun c -> c.Name = "m_Entries" && c.Type = "Entry"))
        |> Seq.choose (fun o -> TypeTreeValue.Get(sf, sfReader, o).TryGetObject() |> toOption)
        |> Seq.tryFind (fun o -> o |> TypeTreeObject.tryGetField "m_Name" = Some "BlueprintReferencedAssets")
        |> Option.bind (TypeTreeObject.toMap >> Map.tryFind "m_Entries")
        |> Option.bind (fun o -> o.TryGetObject() |> toOption)
        |> Option.bind (TypeTreeObject.toMap >> Map.tryFind "Array")
        |> Option.bind (fun o -> o.TryGetArray<ITypeTreeObject>() |> toOption)
        |> Option.toArray
        |> Seq.collect id
        |> Seq.choose (fun o ->
            let assetId = o |> TypeTreeObject.tryGetField<string> "AssetId"
            let fileId = o |> TypeTreeObject.tryGetField<int64> "FileId"
            let asset = o |> TypeTreeObject.tryGetField<PPtr> "Asset"

            match assetId, fileId, asset with
            | Some assetId, Some fileId, Some pptr -> Some (pptr, (assetId, fileId))
            | _ -> None)
        |> Seq.toArray
    
    let getAllBlueprintReferencedAssets() =
        if blueprintReferencedAssets |> Map.isEmpty then
            let objects =
                archives.Value
                |> Seq.collect (fun (_, a) -> a.Nodes)
                |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
                |> Seq.map (fun n -> sprintf "%s%s" mountPoint n.Path)
                |> Seq.choose (fun path -> getSerializedFile path |> toOption)
                |> Seq.map (fun sf -> getBlueprintReferencedAssets sf)
                |> Seq.where (fun assets -> assets.Length > 0)
                |> Seq.toArray

            printfn "Got BlueprintReferencedAssets x %i" objects.Length
            
            blueprintReferencedAssets <-
                objects
                |> Seq.collect id
                |> Seq.map (fun (pptr, (assetID, fileID)) ->
                    (pptr.GetReferencePath(getSerializedFile), pptr.PathID), { AssetID = assetID; FileID = fileID }
                )
                |> Map.ofSeq

        blueprintReferencedAssets

    let getAllContainerMaps() =
        if containersMap |> Map.isEmpty then
            containersMap <-
                archives.Value
                |> Seq.collect (fun (_, a) -> a.Nodes)
                |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
                |> Seq.map (fun n -> sprintf "%s%s" mountPoint n.Path)
                |> Seq.choose (fun path -> getSerializedFile path |> toOption)
                |> Seq.map (fun sf -> sf, getContainerMap sf)
                |> Seq.collect (fun (sf, values) ->
                    values
                    |> Seq.map (fun (pptr, container) -> (pptr.GetReferencePath(getSerializedFile), pptr.PathID), {| SerializedFile = sf.Path; Container = container |}))
                |> Map.ofSeq

        containersMap

    let decodeTexture (texture : Texture2D) =
        let buffer = Array.zeroCreate(4 * texture.Width * texture.Height)
        if Texture2DConverter.DecodeTexture2D(texture, System.Span(buffer), getReader >> MicroOption.Some) then
            Some buffer
        else None

    let mutable textures : Map<(string * int64), SpriteTexture> = Map.empty

    let getTexture (pptr : PPtr) =
        getSerializedFile pptr.SerializedFilePath
        |> toOption
        |> Option.bind (fileIDToPath pptr.FileID)
        |> Option.bind (fun filePath ->
            let key = (filePath, pptr.PathID)

            textures
            |> Map.tryFind key
            |> Option.orElseWith (fun () ->

                #if DEBUG
                printfn $"Load texture PPtr: {pptr.SerializedFilePath} -> FileID = {pptr.FileID}, PathID = {pptr.PathID}"
                #endif

                // let opt : Option<ITypeTreeValue> = pptr.TryDereference(getSerializedFile, getReader >> MicroOption.Some)
                // opt
                dereference pptr
                |> Option.bind (
                    function
                    | :? Texture2D as texture -> 
                        decodeTexture texture
                        |> Option.map (fun bytes -> new SpriteTexture(bytes, Avalonia.PixelSize(texture.Width, texture.Height)))
                    | _ -> None))
                |> Option.map (fun texture ->
                    textures <- textures |> Map.add key texture
                    texture)
        )

    let getSpriteObjects (sf : SerializedFile) =
        sf.Objects
        |> Seq.where (fun o -> sf.GetTypeTreeRoot(o.Id).Type = "Sprite")
        |> Seq.toArray

    let getSprite (objectInfo : ObjectInfo) (sf : SerializedFile) =
        let sfReader = getReader sf.Path

        TypeTreeValue.Get(sf, sfReader, objectInfo)
        |> function
        | :? Parsers.Sprite as s -> Some (s, objectInfo)
        | _ -> None
        |> Option.map (fun (s, o) ->
            let name =
                match s.ToDictionary().TryGetValue("m_Name") with
                | true, name ->
                    Some (name.GetValue<string>()) 
                | _ -> None

            let atlas =
                s.AtlasPtr
                |> MicroOption.op_Implicit
                |> function
                | Some ap when ap <> PPtr.NullPtr ->
                    dereference ap
                    // ap.TryDereference(getSerializedFile, getReader >> MicroOption.Some)
                    // |> toOption
                    |> Option.bind (function :? Parsers.SpriteAtlas as sa -> Some sa | _ -> None)
                | _ -> None

            #if DEBUG
            name
            |> Option.defaultValue ""
            |> printfn "Load sprite \"%s\""
            #endif

            let renderDataKey = s.RenderDataKey |> toOption

            let sprite =
                match atlas with
                | Some atlas ->
                    renderDataKey
                    |> Option.bind (fun rdk ->
                        let succ, sad = atlas.RenderDataMap.TryGetValue(rdk)

                        if succ then Some sad else None)
                    |> Option.bind (fun sad ->
                        let texture = sad.Texture |> getTexture
                        let rect = sad.TextureRect
                        texture
                        |> Option.map (fun texture -> texture, rect))
                    |> Option.map (fun (texture, rect) -> Sprite.Create(texture, rect |> toPixelRect))
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
                        
                        Sprite.Create(texture, rect)
                    )
            sprite
            |> Option.map (fun sprite ->
                { sprite with
                    Name = name
                    RenderDataKey = renderDataKey
                    SerializedFile = sf.Path
                    PathID = o.Id
                    Container =
                        getAllContainerMaps()
                        |> Map.tryFind (sprite.SerializedFile, o.Id)
                        |> Option.map (fun c -> c.Container)
                        |> Option.defaultValue ""

                    BlueprintReference =
                        getAllBlueprintReferencedAssets()
                        |> Map.tryFind (sf.Path, o.Id)
                        |> Option.map (fun assetRef -> assetRef.AssetID, assetRef.FileID)
                }
            )
        )
