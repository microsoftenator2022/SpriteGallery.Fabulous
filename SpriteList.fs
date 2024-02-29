[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteList

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous

let view (model : Model) =
    (ScrollViewer(
        (VStack() {
            for (bitmap, rect, name) in model.Sprites do
                (VStack() {
                    TextBlock($"{name}").centerText()

                    Image(Avalonia.Media.Stretch.None, CroppedBitmap(bitmap, rect)).renderTransform(ScaleTransform(1, -1))
                }).centerHorizontal()

                Separator()
        }).centerHorizontal()
    ))
