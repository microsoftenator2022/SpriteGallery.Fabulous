[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteGrid

open Avalonia.Layout
open Avalonia.Media

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous

[<Struct>]
type SpriteCell = { CellSpan : int; Sprite : Sprite option; ColumnIndex : int; RowIndex : int }

let tileSize = 64
let arBias = 0.25


let generateLayout (sprites : Sprite seq) columns =
    printfn "generating %i column layout" columns
    let layout =
        seq {
            let mutable ci = 0
            let mutable ri = 0

            for sprite in sprites do
                let rect = sprite.Rect
                let aspectRatio = (rect.Width |> float) / (rect.Height |> float)

                let cellSpan =
                    if rect.Width > tileSize then
                        max 1 (ceil (aspectRatio - arBias) |> int)
                    else 1

                // if cellSpan > 1 then
                //     printfn "%s: ratio %f (%i x %i) = %i cells" n aspectRatio rect.Width rect.Height cellSpan

                if ci + cellSpan >= columns then
                    ci <- 0
                    ri <- ri + 1

                yield { Sprite = Some sprite; CellSpan = cellSpan; ColumnIndex = ci; RowIndex = ri }

                ci <- ci + cellSpan
        }
        |> Seq.toArray

    let rows =
        if layout.Length > 0 then
            (layout |> Array.last).RowIndex + 1
        else 0

    printfn "%i rows" rows

    layout

type Msg =
| Unit
| Resize of Avalonia.Size

type Model = Avalonia.Size

let init (sprites : SpritesData) = sprites, Avalonia.Size()

let update (msg : Msg) (model : SpritesData * Model) =
    match msg, model with
    | Unit, _ -> model
    | Resize size, (bm, _) -> bm, size

let view (spritesData : SpritesData, windowSize : Avalonia.Size) =
    let mutable i = 0

    let padding = 2
    let cellSize = tileSize + (padding * 2)

    let columns = 
        windowSize.Width / Avalonia.PixelSize(cellSize, cellSize).ToSizeWithDpi(96).Width + 1.0 |> int

    (ScrollViewer(
        (VStack() {
            View.lazy'
                (fun _ ->
                    let cells = generateLayout (spritesData.Sprites |> Seq.map (fun (s, _) -> s)) columns

                    let rows =
                        if cells.Length > 0 then
                            (cells |> Seq.last).RowIndex + 1
                        else 0
                    
                    (Grid(coldefs = Array.create columns (Pixel cellSize), rowdefs = Array.create rows (Pixel cellSize)) {
                        for (sprite, cellSpan, column, row) in cells |> Seq.choose (fun c -> c.Sprite |> Option.map (fun s -> s, c.CellSpan, c.ColumnIndex, c.RowIndex)) do
                            let image =
                                Image(Stretch.Uniform, CroppedBitmap(sprite.Texture.Bitmap.Force(), sprite.Rect))
                                    .renderTransform(ScaleTransform(1, -1))
                                    .horizontalAlignment(HorizontalAlignment.Center)
                                    .centerVertical()

                            let imageWithBorder =
                                Border(image)
                                    .gridColumnSpan(cellSpan)
                                    .gridColumn(column)
                                    .gridRow(row)
                                    .background(Colors.Transparent)

                            if i = 10 then
                                imageWithBorder
                                    .borderThickness(4.0)
                                    .borderBrush(SolidColorBrush(Colors.Blue))
                                    .margin(-2.0)    
                            else
                                imageWithBorder
                                    .borderThickness(0.0)
                                    .margin(2.0)

                            i <- i + 1
                    })
                )
                (columns, spritesData.Sprites.Length)
        })
    ))
