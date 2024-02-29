[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteGrid

open Avalonia.Layout
open Avalonia.Media
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous

type private SpriteCell = { CellSpan : int; Sprite : Sprite option; ColumnIndex : int; RowIndex : int }

let view (model : Model) =
    let tileSize = 64
    let columns = 16

    let cells =
        seq {
            let mutable ci = 0
            let mutable ri = 0

            for (bitmap, rect, n) in model.Sprites do
                let aspectRatio = (rect.Width |> float) / (rect.Height |> float)
                let cellSpan =
                    if rect.Width > tileSize then
                        ceil aspectRatio |> int
                    else 1

                if ci + cellSpan >= columns then
                    ci <- 0
                    ri <- ri + 1

                yield { Sprite = Some { Bitmap = bitmap; TextureRect = rect }; CellSpan = cellSpan; ColumnIndex = ci; RowIndex = ri }

                ci <- ci + cellSpan
        }
        |> Seq.toArray

    let rows = (cells |> Seq.last).RowIndex + 1

    let mutable i = 0

    let padding = 2

    let cellSize = tileSize + (padding * 2)

    (ScrollViewer(
        (Grid(coldefs = Array.create columns (Pixel cellSize), rowdefs = Array.create rows (Pixel cellSize)) {
            for (bitmap, rect, cellSpan, column, row) in cells |> Seq.choose (fun c -> c.Sprite |> Option.map (fun s -> s.Bitmap, s.TextureRect, c.CellSpan, c.ColumnIndex, c.RowIndex)) do
                let image =
                    Image(Stretch.Uniform, CroppedBitmap(bitmap, rect))
                        .renderTransform(ScaleTransform(1, -1))
                        .horizontalAlignment(HorizontalAlignment.Center)
                        .centerVertical()
                
                let imageWithBorder =
                    Border(image)
                        .gridColumnSpan(cellSpan)
                        .gridColumn(column)
                        .gridRow(row)

                if i = 10 then
                    imageWithBorder
                        .borderThickness(4.0)
                        .borderBrush(SolidColorBrush(Colors.Blue))
                        .margin(-2.0)    
                else
                    imageWithBorder
                        .borderThickness(0.0)
                        .padding(2.0)

                i <- i + 1
        })
    ))
