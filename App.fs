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

type AppView =
| SpriteGridView
| LoadProgressView
| SpriteListView

type Model =
  { Sprites : SpritesData
    CurrentView : AppView
    SpriteGrid : SpriteGrid.Model
    SpriteList : SpriteList.Model
    LoadProgress : LoadProgress.Model
    WindowColors : WindowColors }
with
    member this.SelectedSprite =
        match this.CurrentView with
        | SpriteGridView -> this.SpriteGrid.SelectedSprite
        | _ -> None

type Msg =
| Unit
| LoadColors
| UpdateSprites
| SpriteGridMsg of SpriteGrid.Msg
| LoadProgressMsg of LoadProgress.Msg
| SpriteListMsg of SpriteList.Msg

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
    match msg, model.CurrentView with
    | LoadColors, _ -> { model with WindowColors = WindowColors.GetColors(windowRef) }, Cmd.none

    | SpriteGridMsg sgMsg, SpriteGridView ->
        let sgModel, cmd = SpriteGrid.update sgMsg { model.SpriteGrid with WindowColors = model.WindowColors }

        { model with SpriteGrid = sgModel }, Cmd.map SpriteGridMsg cmd
    | SpriteListMsg slMsg, SpriteListView ->
        let slModel, cmd = SpriteList.update slMsg { model.SpriteList with WindowColors = model.WindowColors }

        { model with SpriteList = slModel }, Cmd.map SpriteListMsg cmd

    | LoadProgressMsg LoadProgress.Complete, LoadProgressView ->
        let model =
            { model with
                Sprites = model.LoadProgress.SpritesData
                CurrentView = SpriteListView
            }

        model,
        SpriteList.UpdateSprites model.Sprites
        |> SpriteListMsg
        |> Cmd.ofMsg

        // SpriteGrid.UpdateSprites model.Sprites
        // |> SpriteGridMsg
        // |> Cmd.ofMsg

    | LoadProgressMsg lpmsg, LoadProgressView ->
        let lbmodel, cmd = LoadProgress.update lpmsg model.LoadProgress
        { model with LoadProgress = lbmodel }, Cmd.map LoadProgressMsg cmd

    | UpdateSprites, SpriteGridView -> model, Cmd.map SpriteGridMsg (model.Sprites |> SpriteGrid.UpdateSprites |> Cmd.ofMsg)
    | _ ->
        eprintfn "WARNING: Unhandled window message %A" msg
        model, Cmd.none

let app (model : Model) =
    let window =
        let view =
            (Dock(true) {
                let details =
                    (View.map (fun _ -> Unit) (SpriteDetailsPanel.view model.SelectedSprite))
                        .margin(12)
                        .style(withAcrylic (acrylicMaterial (model.WindowColors.PanelAcrylicColorOrDefault)))

                let spriteGrid sgm =
                    (View.map SpriteGridMsg (SpriteGrid.view sgm))
                        .margin(4)

                let spriteList slm =
                    (View.map SpriteListMsg (SpriteList.view slm))

                let loadProgress lpm =
                    View.map LoadProgressMsg (LoadProgress.view lpm)

                let spriteView content =
                    SplitView(details, content)
                        .displayMode(SplitViewDisplayMode.Inline)
                        .isPaneOpen(true)
                        .panePlacement(SplitViewPanePlacement.Right)
                        .openPaneLength(450)
                        .paneBackground(Avalonia.Media.Colors.Transparent)

                match model.CurrentView with
                | LoadProgressView ->
                    loadProgress model.LoadProgress
                | SpriteGridView ->
                    model.SpriteGrid |> spriteGrid |> spriteView
                | SpriteListView ->
                    model.SpriteList |> spriteList |> spriteView

            })
                .style(withAcrylic (acrylicMaterial (model.WindowColors.AcrylicColorOrDefault)))

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

    let spriteGrid = SpriteGrid.init (SpritesData.init()) windowRef colors
    let spriteList = SpriteList.init (SpritesData.init()) colors
    let loadProgress = LoadProgress.init windowRef

    let model =
        {
            Sprites = spritesData
            WindowColors = colors
            LoadProgress = loadProgress
            SpriteGrid = spriteGrid
            SpriteList = spriteList
            CurrentView = LoadProgressView
        }
    
    model, Cmd.none
let theme = FluentTheme()
let program = Program.statefulWithCmd init update app
