[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.SpriteList

open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous
open SpriteGallery.Fabulous.Common

type Model = 
  { SpritesData : SpritesData
    SelectedSpriteIndex : int
    WindowColors : WindowColors
    HeaderColDefs : Dimension seq
    UpdateHeader : bool }

let init spritesData windowColors =
  { SpritesData = spritesData
    SelectedSpriteIndex = -1
    WindowColors = windowColors
    HeaderColDefs = [Pixel tileSize; Star; Star; Stars 2]
    UpdateHeader = false }

type Msg =
| Unit
| UpdateSprites of SpritesData
| SpriteSelected of int
| ScrollToSprite of int
| KeyPress of KeyEventArgs
| LayoutUpdated

let margin = 4

let gridRef = ViewRef<Avalonia.Controls.Grid>()

let getHeaderColDefs() =
    let cds = 
        gridRef.TryValue
        |> Option.bind (fun grid ->
            // grid.Children |> Seq.length |> printfn "Grid has %i controls"

            let getColumn i =
                let cs = grid.Children |> Seq.where (fun c -> Avalonia.Controls.Grid.GetColumn(c) = i)
                if cs |> Seq.isEmpty then None else Some cs

            match getColumn 1, getColumn 2, getColumn 3, getColumn 4 with
            | Some c1, Some c2, Some c3, Some c4 ->
                let columnWidth (c : Avalonia.Controls.Control seq) = c |> Seq.map (fun c -> c.DesiredSize.Width) |> Seq.append [0.0] |> Seq.max

                [ float tileSize; columnWidth c1; columnWidth c2; (columnWidth c3) + (columnWidth c4) ]
                |> Seq.map Pixel
                |> Some
            | _ -> None
            )

    // printfn "%A" cds

    cds

let update msg model =
    match msg with
    | Unit -> model, Cmd.none
    | UpdateSprites sprites -> { model with SpritesData = sprites; SelectedSpriteIndex = -1; UpdateHeader = true }, Cmd.none
    | LayoutUpdated ->
        let model =
            if model.UpdateHeader then
                match getHeaderColDefs() with
                | Some coldefs -> { model with HeaderColDefs = coldefs; UpdateHeader = false }
                | None -> model
            else model
                
        model, Cmd.none
    | _ ->
        eprintfn "WARNING: Unhandled SpriteList message %A" msg
        model, Cmd.none

let view (model : Model) =
    (Dock(true) {
        (VStack() {
            (HStack() {
                TextBlock("Filter:")
                    .margin(margin)
                    .verticalAlignment(VerticalAlignment.Center)
                
                TextBox("", fun _ -> Unit)
                    .width(200)
                    .margin(margin)
                    .verticalAlignment(VerticalAlignment.Center)

            })
                .horizontalAlignment(HorizontalAlignment.Left)

            (Grid(coldefs = model.HeaderColDefs, rowdefs = [Auto]) {
                TextBlock("Name")
                    .fontWeight(FontWeight.Bold)
                    .textAlignment(TextAlignment.Center)
                    .margin(margin)
                    .gridRow(0)
                    .gridColumn(1)

                TextBlock("Container")
                    .fontWeight(FontWeight.Bold)
                    .textAlignment(TextAlignment.Center)
                    .margin(margin)
                    .gridRow(0)
                    .gridColumn(2)
                    
                TextBlock("BlueprintReference")
                    .fontWeight(FontWeight.Bold)
                    .textAlignment(TextAlignment.Center)
                    .margin(margin)
                    .gridRow(0)
                    .gridColumn(3)
            })
                .showGridLines(true)
        })
            .dock(Avalonia.Controls.Dock.Top)

        ScrollViewer(
            let rowdefs =
                Array.create model.SpritesData.Sprites.Length (Pixel tileSize)

            (Grid(coldefs = [Pixel tileSize; Auto; Auto; Auto; Auto], rowdefs = rowdefs) {
                for (index, sprite) in (model.SpritesData.Sprites |> Seq.mapi (fun i sprite -> i, sprite)) do
                    let bitmap = sprite.GetHeightScaledBitmap(tileSize)
                    ViewBox(
                        Image(bitmap, Stretch.Uniform)
                            .size(bitmap.Size.Width, bitmap.Size.Height)
                    )
                        .gridRow(index)
                        .gridColumn(0)
                        .stretchDirection(StretchDirection.DownOnly)

                    TextBlock(sprite.Name |> Option.defaultValue "")
                        .margin(margin)
                        .centerVertical()
                        .gridRow(index)
                        .gridColumn(1)
                        
                    TextBlock(sprite.Container)
                        .margin(margin)
                        .centerVertical()
                        .gridRow(index)
                        .gridColumn(2)

                    TextBlock(sprite.BlueprintReference |> Option.map fst |> Option.defaultValue "")
                        .margin(margin)
                        .centerVertical()
                        .gridRow(index)
                        .gridColumn(3)

                    TextBlock(sprite.BlueprintReference |> Option.map snd |> Option.map (sprintf "%i") |> Option.defaultValue "")
                        .margin(margin)
                        .centerVertical()
                        .gridRow(index)
                        .gridColumn(4)
            })
                .showGridLines(model.SpritesData.Sprites.Length > 0)
                .onLayoutUpdated(LayoutUpdated)
                .reference(gridRef)
        )
            .horizontalScrollBarVisibility(Avalonia.Controls.Primitives.ScrollBarVisibility.Disabled)
    })
        .horizontalAlignment(HorizontalAlignment.Stretch)
