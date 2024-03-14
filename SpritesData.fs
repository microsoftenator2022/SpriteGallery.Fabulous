namespace SpriteGallery.Fabulous

open UnityDataTools.FileSystem

type SpritesData =
    {
        Textures : Map<(int * int64), SpriteTexture>
        Sprites : Sprite array
    }

module SpritesData =
    let init() = { Textures = Map.empty; Sprites = Array.empty }

    let getResult (sg : SpriteGetter) =
        match (sg.Textures, sg.Sprites) with
        | Some textures, Some sprites ->
            {
                Textures = textures
                Sprites = sprites |> Seq.toArray
            }
        | _ -> { Textures = Map.empty; Sprites = [||] }

    let loadFromAsync (archiveFile : string) =
        let sg = new SpriteGetter(archiveFile)
        let getAsync =
            async {
                let! _ = sg.GetAsync()

                return getResult sg
            }

        getAsync, sg

    let loadFrom (archiveFile : string) =
        let getAsync, sg = loadFromAsync archiveFile
        use _ = sg

        getAsync
        |> Async.RunSynchronously
