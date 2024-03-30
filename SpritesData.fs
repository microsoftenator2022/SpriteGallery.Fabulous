namespace SpriteGallery.Fabulous

open UnityDataTools.FileSystem

open SpriteGallery.Fabulous.Common

type SpritesData =
    {
        Textures : Map<(string * int64), SpriteTexture>
        Sprites : Sprite list
    }

module SpritesData =
    let init() = { Textures = Map.empty; Sprites = [] }

