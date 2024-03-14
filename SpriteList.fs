[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteList

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous

let view (model : SpritesData) =
    (ScrollViewer(
        (VStack() {
            for sprite in model.Sprites do
                let name = sprite.Name
                (VStack() {
                    TextBlock($"{name}").centerText()

                    Image(Avalonia.Media.Stretch.None, CroppedBitmap(sprite.Texture.Bitmap.Force(), sprite.Rect)).renderTransform(ScaleTransform(1, -1))
                }).centerHorizontal()

                Separator()
        }).centerHorizontal()
    ))
