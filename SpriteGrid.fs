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
    // printfn "generating %i column layout" columns
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

    // printfn "%i rows" rows

    layout

type Msg =
| Unit
| UpdateSprites of SpritesData
| Resize of Avalonia.Size
| SpriteSelected of int

type Model =
  { SpritesData : SpritesData
    ContentSize : Avalonia.Size
    SelectedSpriteIndex : int option }
with
    member this.SelectedSprite = this.SelectedSpriteIndex |> Option.map (fun i -> this.SpritesData.Sprites[i])

let init (sprites : SpritesData) = { SpritesData = sprites; ContentSize = Avalonia.Size(); SelectedSpriteIndex = None }

let update (msg : Msg) (model : Model) =
    match msg with
    | Unit -> model, Cmd.none
    | UpdateSprites spritesData -> { model with SelectedSpriteIndex = None; SpritesData = spritesData }, Cmd.none
    | Resize size -> { model with ContentSize = size }, Cmd.none
    | SpriteSelected index ->
        let model = 
            { model with
                SelectedSpriteIndex =
                    if model.SelectedSpriteIndex <> Some index then
                        Some index
                    else None
            }

        // model.SelectedSpriteIndex
        // |> Option.map (fun i -> i, model.SpritesData.Sprites[i])
        // |> printfn "selected sprite %A"

        model, Cmd.none

let view (model : Model) =
    let mutable i = 0

    let padding = 2
    let cellSize = tileSize + (padding * 2)

    // let getColumns width = width / Avalonia.PixelSize(cellSize, cellSize).ToSizeWithDpi(96).Width + 1.0 |> int

    let columns = model.ContentSize.Width / Avalonia.PixelSize(cellSize, cellSize).ToSizeWithDpi(96).Width + 1.0 |> int

    (ScrollViewer(
        (VStack() {
            View.lazy'
                (fun _ ->
                    let cells = generateLayout model.SpritesData.Sprites columns

                    let rows =
                        if cells.Length > 0 then
                            (cells |> Seq.last).RowIndex + 1
                        else 0
                    
                    (Grid(coldefs = Array.create columns (Pixel cellSize), rowdefs = Array.create rows (Pixel cellSize)) {
                        for (sprite, cellSpan, column, row) in cells |> Seq.choose (fun c -> c.Sprite |> Option.map (fun s -> s, c.CellSpan, c.ColumnIndex, c.RowIndex)) do
                            let index = i
                            let image =
                                Image(Stretch.Uniform, CroppedBitmap(sprite.Texture.Bitmap.Force(), sprite.Rect))
                                    .renderTransform(ScaleTransform(1, -1))
                                    .centerHorizontal()
                                    .centerVertical()
                                    .stretchDirection(StretchDirection.DownOnly)
                                    .onTapped(fun _ -> SpriteSelected index)

                            let imageWithBorder =
                                Border(image)
                                    .gridColumnSpan(cellSpan)
                                    .gridColumn(column)
                                    .gridRow(row)
                                    .background(Colors.Transparent)
                                    
                            if model.SelectedSpriteIndex = Some index then
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
                        .onSizeChanged(fun args ->
                            if args.WidthChanged then
                                args.NewSize |> Resize
                            else Unit)
                )
                (columns, model.SelectedSpriteIndex, model.SpritesData.Sprites.Length)
        })
    ))
