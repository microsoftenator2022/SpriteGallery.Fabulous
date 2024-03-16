namespace SpriteGallery.Fabulous

open UnityDataTools.FileSystem

type SpritesData =
    {
        Textures : Map<(int * int64), SpriteTexture>
        Sprites : Sprite array
    }

module Texture =
    let copyRect (textureBytes : System.Span<byte>) stride (rect : Avalonia.PixelRect) (dest : System.Span<byte>) =
        for n in 0..(rect.Height - 1) do
            let line = textureBytes.Slice((n + rect.Y) * stride, stride)
            let xBytes = rect.X * 4
            let widthBytes = rect.Width * 4

            let source = line.Slice(xBytes, widthBytes)
            let destLine = dest.Slice((rect.Height - n - 1) * widthBytes, widthBytes)
            
            source.CopyTo(destLine)

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
