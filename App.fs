module SpriteGallery.Fabulous.App

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Platform
open Avalonia.Media.Imaging
open Avalonia.Themes.Fluent

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous.Common
open SpriteGallery.Fabulous.Views

let windowRef = ViewRef<Window>()

type ViewModel =
    | Empty
    | SpriteGridModel of SpriteGrid.Model
    // | LoadProgressModel of LoadProgress.LoadBundleView.Model
    | LoadProgressModel of LoadProgress.Model

type Model =
  { Sprites : SpritesData
    ViewModel : ViewModel
    // LoadProgress : LoadProgress.Model
    WindowColors : WindowColors }
with
    member this.SelectedSprite =
        match this.ViewModel with
        | SpriteGridModel sgModel -> sgModel.SelectedSprite
        | _ -> None

type Msg =
| Unit
| LoadColors
| UpdateSprites
| SpriteGridMsg of SpriteGrid.Msg
// | LoadProgressMsg of LoadProgress.Msg
| LoadProgressMsg of LoadProgress.Msg

let mutable (icon : Bitmap option) = None

let tryGetAppIcon() =
    icon <-
        icon
        |> Option.orElseWith (fun () ->
            let resourceName = "SpriteGallery.Fabulous.owlcat_suspecting_framed.png"

            let ass = System.Reflection.Assembly.GetExecutingAssembly()
            
            if ass.GetManifestResourceNames() |> Seq.contains resourceName then
                let stream = ass.GetManifestResourceStream(resourceName)
                let bitmap = new Avalonia.Media.Imaging.Bitmap(stream)

                stream.Dispose()

                Some bitmap
            else None
        )
    icon

let update (msg : Msg) (model : Model) =
    match msg, model.ViewModel with
    | LoadColors, _ ->
        { model with WindowColors = WindowColors.GetColors(windowRef) }, Cmd.none
    | SpriteGridMsg sgMsg, SpriteGridModel sgModel ->
        let sgModel, cmd = SpriteGrid.update sgMsg { sgModel with WindowColors = model.WindowColors }

        { model with ViewModel = SpriteGridModel sgModel; }, Cmd.map SpriteGridMsg cmd

    // | LoadProgressMsg lpMsg, _ ->
    //     let lpModel, cmd = LoadProgress.update lpMsg model.LoadProgress

    //     let sprites = model.Sprites

    //     let model =
    //         { model with
    //             LoadProgress = lpModel
    //             Sprites = 
    //                 match lpModel.Sprites with
    //                 | Some spritesData -> spritesData
    //                 | None -> SpritesData.init()
    //         }

    //     model,
    //     if sprites <> model.Sprites then
    //         Cmd.batch [Cmd.map LoadProgressMsg cmd; Cmd.ofMsg UpdateSprites]
    //     else Cmd.map LoadProgressMsg cmd
    | LoadProgressMsg LoadProgress.Complete, LoadProgressModel lbmodel ->
        { model with
            Sprites = lbmodel.SpritesData
            ViewModel = SpriteGrid.init (SpritesData.init()) windowRef model.WindowColors |> SpriteGridModel
        },
        SpriteGrid.UpdateSprites lbmodel.SpritesData
        |> SpriteGridMsg
        |> Cmd.ofMsg

    | LoadProgressMsg lpmsg, LoadProgressModel lbmodel ->
        let lbmodel, cmd = LoadProgress.update lpmsg lbmodel
        { model with ViewModel = LoadProgressModel lbmodel }, Cmd.map LoadProgressMsg cmd

    | UpdateSprites, viewModel ->
        match viewModel with
        | SpriteGridModel _ ->
            model, Cmd.map SpriteGridMsg (model.Sprites |> SpriteGrid.UpdateSprites |> Cmd.ofMsg)
        | _ -> model, Cmd.none
    | _, Empty -> model, Cmd.none
    | _ -> model, Cmd.none

let app (model : Model) =
    let window =
        let view =
            (Panel() {
                // let loadProgress = 
                //     View.map LoadProgressMsg (LoadProgress.view model.LoadProgress)

                let content =
                    (Dock(true) {
                        // loadProgress
                        //     .dock(Dock.Bottom)

                        match model.ViewModel with
                        | SpriteGridModel sgm ->
                            View.map SpriteGridMsg (SpriteGrid.view sgm)
                        | LoadProgressModel lpModel ->
                            View.map LoadProgressMsg (LoadProgress.view lpModel)
                        | Empty -> ()
                    })
                        .margin(4)

                let details =
                    (View.map (fun _ -> Unit) (SpriteDetailsPanel.view model.SelectedSprite))
                        .margin(4)
                    |> withAcrylic (acrylicMaterial (model.WindowColors.PanelAcrylicColorOrDefault))
                
                SplitView(details, content)
                    .displayMode(SplitViewDisplayMode.Inline)
                    .isPaneOpen(true)
                    .panePlacement(SplitViewPanePlacement.Right)
                    .openPaneLength(450)
                    .paneBackground(Avalonia.Media.Colors.Transparent)

            })
            |> withAcrylic (acrylicMaterial (model.WindowColors.AcrylicColorOrDefault))

        Window(view)
            .reference(windowRef)
            .transparencyLevelHint([WindowTransparencyLevel.AcrylicBlur])
            .background(Avalonia.Media.Colors.Transparent)
            .onOpened(LoadColors)

    let window =
        match tryGetAppIcon() with
        | Some icon ->
            window.icon(icon)
        | None -> window

    DesktopApplication(window)

let init () =
    let spritesData = SpritesData.init()
    let colors = WindowColors.GetColors windowRef

    let model =
        {
            Sprites = spritesData
            // ViewModel = SpriteGrid.init spritesData windowRef colors |> SpriteGridModel
            ViewModel = LoadProgress.init windowRef |> LoadProgressModel
            // LoadProgress = LoadProgress.init windowRef
            WindowColors = colors
        }
    
    model, Cmd.none
let theme = FluentTheme()
let program = Program.statefulWithCmd init update app
