module SpriteGallery.Fabulous.SpriteDetailsPanel

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous

let view (sprite : Sprite option) =
    (Dock(true) {
        (VStack() {
            let margin = 4

            let setLabelTextStyle (widget : WidgetBuilder<_, IFabTextBlock>) =
                widget
                    .margin(margin)
                    .centerVertical()
                    .horizontalAlignment(HorizontalAlignment.Left)

            let setValueStyle (widget : WidgetBuilder<_, IFabTextBox>) =
                widget
                    .isReadOnly(true)
                    .margin(margin)
                    .centerVertical()
                    .horizontalAlignment(HorizontalAlignment.Stretch)

            (Grid(coldefs = [Auto; Auto], rowdefs = [Auto; Auto]) {
                (TextBlock("Width")
                |> setLabelTextStyle)
                    .gridRow(0)
                    .gridColumn(0)
                    .margin(margin, margin, margin, margin / 4 |> float)
                
                (TextBox(sprite |> Option.map (fun s -> s.Rect.Width.ToString()) |> Option.defaultValue "", fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(0)
                    .gridColumn(1)
                    .margin(margin, margin, margin, margin / 4 |> float)

                (TextBlock("Height")
                |> setLabelTextStyle)
                    .gridRow(1)
                    .gridColumn(0)
                    .margin(margin, margin / 4 |> float, margin, margin)
                
                (TextBox(sprite |> Option.map (fun s -> s.Rect.Height.ToString()) |> Option.defaultValue "", fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(1)
                    .gridColumn(1)
                    .margin(margin, margin / 4 |> float, margin, margin)
            })
                .horizontalAlignment(HorizontalAlignment.Right)

            (Grid(coldefs = [Auto; Star], rowdefs = [Auto; Auto; Auto; Auto; Auto]) {
                (TextBlock("Name")
                |> setLabelTextStyle)
                    .gridRow(0)
                    .gridColumn(0)
                    
                (TextBox(sprite |> Option.bind (fun s -> s.Name) |> Option.defaultValue "", fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(0)
                    .gridColumn(1)

                (TextBlock("Container")
                |> setLabelTextStyle)
                    .gridRow(1)
                    .gridColumn(0)

                let container = sprite |> Option.map (fun s -> s.Container) |> Option.defaultValue ""

                (TextBox(container, fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(1)
                    .gridColumn(1)

                (TextBlock("PathID")
                |> setLabelTextStyle)
                    .gridRow(2)
                    .gridColumn(0)

                let pathID = sprite |> Option.map (fun s -> s.PathID.ToString()) |> Option.defaultValue ""

                (TextBox(pathID, fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(2)
                    .gridColumn(1)

                (TextBlock("RenderDataKey")
                |> setLabelTextStyle)
                    .gridRow(3)
                    .gridRowSpan(2)
                    .gridColumn(0)

                let renderDataKey = sprite |> Option.bind (fun s -> s.RenderDataKey)

                (TextBox(renderDataKey |> Option.map (fun struct (guid, _) -> guid.ToString("n")) |> Option.defaultValue "", fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(3)
                    .gridColumn(1)
                    .margin(margin, margin, margin, margin / 4 |> float)
                
                (TextBox(renderDataKey |> Option.map (fun struct (_, fid) -> fid.ToString()) |> Option.defaultValue "", fun _ -> Cmd.none)
                |> setValueStyle)
                    .gridRow(4)
                    .gridColumn(1)
                    .margin(margin, margin / 4 |> float, margin, margin)
            })
                .horizontalAlignment(HorizontalAlignment.Stretch)
        })
            .dock(Dock.Bottom)
        
        ViewBox(
            match sprite with
            | Some sprite ->
                Image(Stretch.Uniform, CroppedBitmap(sprite.Texture.Bitmap.Force(), sprite.Rect))
                    .renderTransform(ScaleTransform(1, -1))
                    .size(sprite.Rect.Width, sprite.Rect.Height)
            | None ->
                Image(DrawingImage(GeometryDrawing(RectangleGeometry(Avalonia.Rect(0, 0, 64, 64)), Colors.Transparent)))
        )
            .center()
            .stretch(Stretch.Uniform)
            .stretchDirection(StretchDirection.DownOnly)
    })
        .horizontalAlignment(HorizontalAlignment.Stretch)