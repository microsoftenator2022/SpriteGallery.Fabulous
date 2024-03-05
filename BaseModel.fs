namespace SpriteGallery.Fabulous

open UnityDataTools.FileSystem

type BaseModel =
    {
        Textures : Map<(int * int64), SpriteGallery.Fabulous.SpriteTexture>
        Sprites : (Sprite * string) list
    }

module BaseModel =
    let init (archiveFile : string) =
        // let archiveFile = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"

        // UnityFileSystem.Init()

        // let bundlesDirectory = System.IO.Path.GetDirectoryName(archiveFile)

        // let dependencylist =
        //     System.IO.Path.Join(bundlesDirectory, "dependencylist.json")

        // let dependencies =
        //     if System.IO.File.Exists dependencylist then
        //         UnityData.getDependencies dependencylist
        //         |> Map.tryFind (System.IO.Path.GetFileName(archiveFile))
        //         |> Option.map (fun files ->
        //             seq {
        //                 for f in files do
        //                     let path = System.IO.Path.Join(bundlesDirectory, f)
        //                     UnityFileSystem.MountArchive(path, UnityData.mountPoint)
        //             }
        //             |> Seq.toArray)
        //         |> function
        //         | Some list -> list
        //         | None -> [||]
        //     else [||]

        // let archive = UnityFileSystem.MountArchive(archiveFile, UnityData.mountPoint)

        // let sfNode =
        //     archive.Nodes
        //     |> Seq.find (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))

        // let sfPath = $"{UnityData.mountPoint}{sfNode.Path}"

        // let textures, sprites = Sprites.get dependencies sfPath

        // let sprites =
        //     sprites
        //     |> Seq.choose (
        //         function
        //         | Some sprite, name ->
        //             Some (sprite, name |> Option.defaultValue "" |> sprintf "%A")
        //         | _ -> None
        //     )

        // archive.Dispose()
        
        // for archive in dependencies do
        //     archive.Dispose()

        // UnityFileSystem.Cleanup()

        use sg = new SpriteGetter(archiveFile)

        sg.GetAsync()
        |> Async.RunSynchronously
        |> ignore

        match (sg.Textures, sg.Sprites) with
        | Some textures, Some sprites ->
            {
                Textures = textures
                Sprites = sprites |> Seq.sortBy (fun (_, name) -> name) |> Seq.toList
            }
        | _ -> { Textures = Map.empty; Sprites = [] }
        