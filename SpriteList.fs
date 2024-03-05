[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteList

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous

let view (model : BaseModel) =
    (ScrollViewer(
        (VStack() {
            for (sprite, name) in model.Sprites do
                (VStack() {
                    TextBlock($"{name}").centerText()

                    Image(Avalonia.Media.Stretch.None, CroppedBitmap(sprite.Texture.Bitmap.Force(), sprite.Rect)).renderTransform(ScaleTransform(1, -1))
                }).centerHorizontal()

                Separator()
        }).centerHorizontal()
    ))
