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

type BlueprintAssetReference = { AssetID : string; FileID : int64 }

[<RequireQualifiedAccessAttribute>]
module AssetLoader =
    let private initReaderCache() = new System.Threading.ThreadLocal<(string * UnityBinaryFileReader)[]>((fun () -> [||]), true)
    let mutable private readers = initReaderCache()
    
    let initSerializedFileCache() = new System.Threading.ThreadLocal<SerializedFile[]>((fun () -> [||]), true)
    let mutable private serializedFiles = initSerializedFileCache()

    let initArchiveCache() = new System.Threading.ThreadLocal<(string * UnityArchive)[]>((fun () -> [||]), true)
    let mutable private archives = initArchiveCache()

    let mutable private blueprintReferencedAssets : Map<(string * int64), BlueprintAssetReference> = Map.empty

    let mutable private objectCache : Map<(string * int64), ITypeTreeValue> = Map.empty

    let mutable textures : Map<(string * int64), SpriteTexture> = Map.empty
    
    let init = UnityFileSystem.Init

    let cleanup() =
        blueprintReferencedAssets <- Map.empty
        objectCache <- Map.empty

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

        textures <- Map.empty

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

        let dependencies = 
            seq {
                yield!
                    dependenciesPaths
                    |> Seq.map (fun path -> mountArchive path)
            }
            |> Seq.toArray

        mountArchive archiveFile, dependencies

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
                serializedFiles.Value <- serializedFiles.Value |> Array.appendOne sf
                Some sf
            else None)
        |> MicroOption.op_Implicit

    let dereference (pptr : PPtr) =
        objectCache
        |> Map.tryFind (pptr.SerializedFilePath, pptr.PathID)
        |> Option.orElseWith (fun () ->
            pptr.TryDereference(getSerializedFile, getReader >> MicroOption.Some)
            |> toOption
            |> Option.bind (fun v -> objectCache <- objectCache |> Map.add (pptr.SerializedFilePath, pptr.PathID) v; Some v))

    let getAssetBundleAsset (sf : SerializedFile) =
        let sfReader = getReader sf.Path
        
        sf.Objects
        |> Seq.tryFind (fun o -> sf.GetTypeTreeRoot(o.Id).Type = "AssetBundle")
        |> Option.bind (fun o -> TypeTreeValue.Get(sf, sfReader, o).TryGetValue<AssetBundle>() |> toOption)
        |> Option.map (fun f -> f.Invoke())

    let mutable containersMaps : Map<string, Map<int64, string>> = Map.empty

    let getContainerMap (sf : SerializedFile) =
        containersMaps
        |> Map.tryFind sf.Path
        |> function
        | Some map -> map
        | None ->
            let map =
                getAssetBundleAsset sf
                |> Option.toArray
                |> Seq.collect (fun ab ->
                    ab.ContainerMap
                    |> Seq.collect (fun cm -> cm.Value |> Seq.map (fun ai -> ai, cm.Key))
                    |> Seq.collect (fun (ai, cid) ->
                        ai.GetAllAssetPPtrs(ab.PreloadTable)
                        |> Seq.append [ai.Asset]
                        |> Seq.distinctBy (fun pptr -> pptr.PathID)
                        |> Seq.map (fun pptr -> pptr.PathID, cid)
                    )
                )
                |> Map.ofSeq

            containersMaps <-
                containersMaps |> Map.add (sf.Path) map

            map

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

            blueprintReferencedAssets <-
                objects
                |> Seq.collect id
                |> Seq.map (fun (pptr, (assetID, fileID)) ->
                    (pptr.GetReferencePath(getSerializedFile), pptr.PathID), { AssetID = assetID; FileID = fileID }
                )
                |> Map.ofSeq

        blueprintReferencedAssets

    let decodeTexture (texture : Texture2D) =
        let buffer = Array.zeroCreate(4 * texture.Width * texture.Height)
        if Texture2DConverter.DecodeTexture2D(texture, System.Span(buffer), getReader >> MicroOption.Some) then
            Some buffer
        else None

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

    let getSpriteObjectsInArchive (archive : UnityArchive) =
        archive.Nodes
        |> Seq.where (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))
        |> Seq.map (fun n -> $"{UnityData.mountPoint}{n.Path}")
        |> Seq.choose (fun path -> getSerializedFile path |> toOption)
        |> Seq.map (fun sf -> sf, getSpriteObjects sf)
        |> Seq.collect (fun (sf, ois) -> ois |> Seq.map (fun oi -> sf.Path, oi))
        |> Seq.toArray

    let getSprite (objectInfo : ObjectInfo) (sf : SerializedFile) =
        let sfReader = getReader sf.Path

        TypeTreeValue.Get(sf, sfReader, objectInfo)
        |> function
        | :? Parsers.Sprite as s -> Some (s, objectInfo)
        | _ -> None
        |> Option.bind (fun (s, o) ->
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
                        getContainerMap sf
                        |> Map.tryFind o.Id
                        |> Option.defaultValue ""
                        // getAllContainerMaps()
                        // |> Map.tryFind (sprite.SerializedFile, o.Id)
                        // |> Option.map (fun c -> c.Container)
                        // |> Option.defaultValue ""

                    BlueprintReference =
                        getAllBlueprintReferencedAssets()
                        |> Map.tryFind (sf.Path, o.Id)
                        |> Option.map (fun assetRef -> assetRef.AssetID, assetRef.FileID)
                }
            )
        )
