[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteGrid

open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous
open SpriteGallery.Fabulous.Common

[<Struct>]
type SpriteCell = { CellSpan : int; Sprite : Sprite option; ColumnIndex : int; RowIndex : int }

let arBias = 0.25

let generateLayout (sprites : Sprite seq) columns tileSize =
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
                        max 1 (min (ceil (aspectRatio - arBias) |> int) (columns - 1))
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
| ScrollToSprite of int
| KeyPress of KeyEventArgs

type Model =
  { SpritesData : SpritesData
    ContentSize : Avalonia.Size
    SelectedSpriteIndex : int option
    HighlightBrush : IBrush
    Layout : SpriteCell array
    ScrollViewer : ViewRef<Avalonia.Controls.ScrollViewer>
    Window : ViewRef<Avalonia.Controls.Window> }
with
    member this.SelectedSprite =
        this.SelectedSpriteIndex
        |> Option.bind (fun i ->
            if i >= 0 && i < this.SpritesData.Sprites.Length then
                this.SpritesData.Sprites[i] |> Some
            else None)

let init (sprites : SpritesData) (window : ViewRef<Avalonia.Controls.Window>) =
  { SpritesData = sprites
    ContentSize = Avalonia.Size()
    SelectedSpriteIndex = None
    HighlightBrush = Brushes.Blue
    Layout = Array.empty
    ScrollViewer = ViewRef<Avalonia.Controls.ScrollViewer>()
    Window = window }

let tileSize = 64
let padding = 2
let cellSize = tileSize + (padding * 2)

let moveSelectionDown layout (currentRow, currentColumn) =
    layout
    |> Seq.pairwise
    |> Seq.tryFindIndex (fun (x, y) ->
        x.RowIndex = currentRow + 1
        && (y.ColumnIndex > currentColumn || y.RowIndex > currentRow + 1)
        && x.ColumnIndex <= currentColumn)
    |> Option.map SpriteSelected

let moveSelectionUp layout (currentRow, currentColumn) =
    layout
    |> Seq.pairwise
    |> Seq.tryFindIndex (fun (x, y) ->
        x.RowIndex = currentRow - 1
        && (y.ColumnIndex > currentColumn || y.RowIndex > currentRow - 1)
        && x.ColumnIndex <= currentColumn)
    |> Option.map SpriteSelected

let moveSelectionLeft (layout : SpriteCell[]) index =
    if index <= 0 then None
    elif layout[index - 1].RowIndex < layout[index].RowIndex then None
    else SpriteSelected (index - 1) |> Some

let moveSelectionRight (layout : SpriteCell[]) index =
    if index >= layout.Length then None
    elif layout[index + 1].RowIndex > layout[index].RowIndex then None
    else SpriteSelected (index + 1) |> Some

let copySpriteToClipboard (window : ViewRef<Avalonia.Controls.Window>) sprite = async {
    match window.TryValue with
    | Some window ->
        let source = System.Span(sprite.Texture.Bytes)
        let stride = sprite.Texture.Bitmap.Force().PixelSize.Width * 4
        let rect = sprite.Rect
        let dest = Array.zeroCreate<byte> (rect.Width * rect.Height * 4)
        Texture.copyRect source stride rect (System.Span(dest))
        
        use bitmap = createBitmap dest sprite.Rect.Size
        let ms = new System.IO.MemoryStream()
        
        bitmap.Save(ms)
        
        let pngBytes = ms.ToArray()

        printfn "%i png bytes" pngBytes.Length
        
        let dataObject = Avalonia.Input.DataObject()
        
        dataObject.Set("image/png", pngBytes)

        let! _ =
            window.Clipboard.SetDataObjectAsync(dataObject)
            |> Async.AwaitTask

        return Some Unit
    | _ -> return None
}
    

let handleKeyPress (args : KeyEventArgs) model =
    let cells = model.Layout
    match model.SelectedSpriteIndex with
    | Some index ->
        let currentColumn = cells[index].ColumnIndex
        let currentRow = cells[index].RowIndex

        if cells |> Seq.length < 2 then Cmd.ofMsg Unit
        else
            match args.KeyModifiers, args.Key with
            | KeyModifiers.Control, Key.C ->
                match model.SelectedSprite with
                | Some sprite ->
                    copySpriteToClipboard model.Window sprite
                    |> Cmd.ofAsyncMsgOption
                | None -> Cmd.none
            | _, Key.Right ->
                moveSelectionRight model.Layout index
                |> Cmd.ofMsgOption
            | _, Key.Left ->
                moveSelectionLeft model.Layout index
                |> Cmd.ofMsgOption
            | _, Key.Up ->
                moveSelectionUp model.Layout (currentRow, currentColumn)
                |> Cmd.ofMsgOption            
            | _, Key.Down ->
                moveSelectionDown model.Layout (currentRow, currentColumn)
                |> Cmd.ofMsgOption            // return
            | _ -> Cmd.none

    | None -> Cmd.none

let update (msg : Msg) (model : Model) =
    let getColumns model =
        model.ContentSize.Width / Avalonia.PixelSize(cellSize, cellSize).ToSizeWithDpi(96).Width + 1.0 |> int |> max 1

    match msg with
    | Unit -> model, Cmd.none
    | KeyPress kea ->
        model, handleKeyPress kea model
    | UpdateSprites spritesData ->
        let model = 
            { model with
                SpritesData = spritesData
                Layout = generateLayout spritesData.Sprites (getColumns model) tileSize
            }

        match model.SelectedSpriteIndex with
        | Some i when i < model.Layout.Length ->
            model, Cmd.none
        | _ -> { model with SelectedSpriteIndex = None }, Cmd.none

    | Resize size -> { model with ContentSize = size }, UpdateSprites model.SpritesData |> Cmd.ofMsg
    | SpriteSelected index ->
        let model = 
            { model with
                SelectedSpriteIndex =
                    if model.SelectedSpriteIndex <> Some index then
                        Some index
                    else None
            }
        
        let cmd =
            if model.SelectedSpriteIndex.IsNone then Cmd.none
            elif index >= model.Layout.Length then Cmd.none

            else
                match model.ScrollViewer.TryValue with
                | Some sv ->
                    let y = sv.Offset.Y

                    let spriteY1 = model.Layout[index].RowIndex * cellSize |> float
                    let spriteY2 = (model.Layout[index].RowIndex + 1) * cellSize |> float

                    if y > spriteY1 || (y + sv.Viewport.Height) < spriteY2 then
                        ScrollToSprite index |> Cmd.ofMsg
                    else
                        Cmd.none
                | None -> Cmd.none

        model, cmd
    | ScrollToSprite index ->
        if index >= model.Layout.Length then ()
        else
            match model.ScrollViewer.TryValue with
            | Some sv ->
                let y = sv.Offset.Y

                let spriteY1 = model.Layout[index].RowIndex * cellSize |> float
                let spriteY2 = (model.Layout[index].RowIndex + 1) * cellSize |> float

                if y > spriteY1 then
                    sv.Offset <- sv.Offset.WithY(spriteY2 - sv.Viewport.Height)
                elif (y + sv.Viewport.Height) < spriteY2 then
                    sv.Offset <- sv.Offset.WithY(spriteY1)
                
            | None -> ()

        model, Cmd.none

let view (model : Model) =
    let columns =
        model.Layout
        |> Seq.where (fun cell -> cell.RowIndex = 0)
        |> Seq.tryLast
        |> Option.map (fun cell -> cell.ColumnIndex + 1)
        |> Option.defaultValue 0

    (ScrollViewer(
        (VStack() {
            View.lazy'
                (fun (columns, selectedSpriteIndex, _) ->
                    let mutable i = 0
                    let cells = model.Layout

                    let rows =
                        if cells.Length > 0 then
                            (cells |> Seq.last).RowIndex + 1
                        else 0

                    (Grid(coldefs = Array.create columns (Pixel cellSize), rowdefs = Array.create rows (Pixel cellSize)) {
                        for (sprite, cellSpan, column, row) in cells |> Seq.choose (fun c -> c.Sprite |> Option.map (fun s -> s, c.CellSpan, c.ColumnIndex, c.RowIndex)) do
                            let index = i

                            let image =
                                ViewBox(
                                    Image(Stretch.Uniform, CroppedBitmap(sprite.Texture.Bitmap.Force(), sprite.Rect))
                                        .renderTransform(ScaleTransform(1, -1))
                                        .size(sprite.Rect.Width, sprite.Rect.Height))
                                    .stretchDirection(StretchDirection.DownOnly)
                            
                            let imageWithBorder =
                                Border(image)
                                    .gridColumnSpan(cellSpan)
                                    .gridColumn(column)
                                    .gridRow(row)
                                    .onTapped(fun _ -> SpriteSelected index)
                                    .background(Colors.Transparent)
                                    
                            if selectedSpriteIndex = Some index then
                                imageWithBorder
                                    .borderThickness(4.0)
                                    .borderBrush(model.HighlightBrush)
                                    .margin(-2.0)
                            else
                                imageWithBorder
                                    .borderThickness(0.0)
                                    .margin(2.0)

                            i <- i + 1
                    })
                        .horizontalAlignment(HorizontalAlignment.Stretch)
                        .focusable(true)
                        .onKeyDown(fun args -> KeyPress args)
                )
                (columns, model.SelectedSpriteIndex, model.SpritesData.Sprites.Length)
        })
            .onSizeChanged(fun args ->
                if args.WidthChanged then
                    args.NewSize |> Resize
                else Unit)
    ))
        .reference(model.ScrollViewer)
