namespace SpriteGallery.Fabulous

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

[<RequireQualifiedAccess>]
module LoadProgress =
    type Model =
      { Complete : bool
        Current : int
        Max : int
        Window : ViewRef<Window>
        Sprites : SpritesData option }

    let init windowRef =
        { Complete = false
          Current = 0
          Max = 1
          Window = windowRef
          Sprites = None }

    type Msg =
    | Unit
    | OpenFile
    | LoadProgress of SpriteGetter
    | UpdateSprites of SpritesData
    
    let update msg model =
        match msg with
        | Unit -> model, Cmd.none
        | OpenFile ->
            model.Window.TryValue
            |> function
            | None ->
                eprintfn "no windowref"
                model, Cmd.none
            | Some window ->
                model,
                Cmd.ofAsyncMsgOption (async {
                    let fpo = Storage.FilePickerOpenOptions()
                    fpo.AllowMultiple <- false

                    let! files =
                        window.StorageProvider.OpenFilePickerAsync(fpo)
                        |> Async.AwaitTask

                    let filePath =
                        files
                        |> Seq.tryHead
                        |> Option.map (fun f -> f.Path.LocalPath)

                    match filePath with
                    | Some path ->
                        let task, sg = SpritesData.loadFromAsync path
                        task |> Async.Ignore |> Async.Start
                        
                        return Some (LoadProgress sg)
                    | None -> return None
                })
        | LoadProgress getter ->
            let (current, max) = getter.Progress

            let model = { model with Sprites = None; Complete = getter.Complete; Current = current; Max = max; }

            if getter.Complete then
                printfn "Done"
                let sprites = SpritesData.getResult getter

                (getter :> System.IDisposable).Dispose()

                model, Cmd.ofMsg(UpdateSprites sprites)
            else
                model, Cmd.ofAsyncMsg (async {
                    let! _ = 
                        Async.AwaitEvent getter.Update

                    return (LoadProgress getter)
                })
        | UpdateSprites spritesData ->
            { model with Sprites = Some spritesData }, Cmd.none

module App =
    type ViewModel =
    | Empty
    | SpriteGridModel of SpriteGrid.Model

    type Model =
      { Sprites : SpritesData
        ViewModel : ViewModel
        LoadProgress : LoadProgress.Model
        Window : ViewRef<Window> }
    with
        member this.SelectedSprite =
            match this.ViewModel with
            | SpriteGridModel sgModel -> sgModel.SelectedSprite
            | _ -> None

    type Msg =
    | Unit
    | UpdateSprites
    | SpriteGridMsg of SpriteGrid.Msg
    | LoadProgressMsg of LoadProgress.Msg

    let tryGetAppIcon() =
        let resourceName = "SpriteGallery.Fabulous.owlcat_suspecting_framed.png"

        let ass = System.Reflection.Assembly.GetExecutingAssembly()
        
        if ass.GetManifestResourceNames() |> Seq.contains resourceName then
            ass.GetManifestResourceStream(resourceName) |> Some
        else None

    let update (msg : Msg) (model : Model) =
        match msg, model.ViewModel with
        | SpriteGridMsg sgMsg, SpriteGridModel sgModel ->
            let sgModel, cmd = SpriteGrid.update sgMsg sgModel

            { model with ViewModel = SpriteGridModel sgModel }, Cmd.map SpriteGridMsg cmd

        | LoadProgressMsg lpMsg, _ ->
            let lpModel, cmd = LoadProgress.update lpMsg model.LoadProgress

            let sprites = model.Sprites

            let model =
                { model with
                    LoadProgress = lpModel
                    Sprites = 
                        match lpModel.Sprites with
                        | Some spritesData -> spritesData
                        | None -> SpritesData.init()
                }

            model,
            if sprites <> model.Sprites then
                Cmd.batch [Cmd.map LoadProgressMsg cmd; Cmd.ofMsg UpdateSprites]
            else Cmd.map LoadProgressMsg cmd

        | UpdateSprites, viewModel ->
            match viewModel with
            | Empty -> model, Cmd.none
            | SpriteGridModel _ ->
                model, Cmd.map SpriteGridMsg (model.Sprites |> SpriteGrid.UpdateSprites |> Cmd.ofMsg)

        | Unit, _ -> model, Cmd.none
        | _, Empty -> model, Cmd.none

    let app (model : Model) =
        let acrylicColor =
            model.Window |> tryGetColor "SystemAltMediumHighColor" |> Option.defaultValue Avalonia.Media.Colors.DimGray
        let panelAcrylicColor =
            model.Window |> tryGetColor "SystemAltMediumColor" |> Option.defaultValue Avalonia.Media.Colors.Gray
        let highlightBrush =
            model.Window |> tryGetThemeResource<Avalonia.Media.IBrush> "SystemControlHighlightAccentBrush" |> Option.defaultValue Avalonia.Media.Brushes.Blue

        let icon =
            tryGetAppIcon()
            |> Option.map (fun stream ->
                let bitmap = new Avalonia.Media.Imaging.Bitmap(stream)

                stream.Dispose()

                bitmap)

        let window =
            let view =
                (Panel() {

                    // (Panel() {
                    //     TextBlock("SpriteGallery")
                    // })
                    //     .margin(8)
                    //     .dock(Dock.Top)
                    //     .isHitTestVisible(false)

                    // match model.Window.Value.TryGetResource("SomeKey") with
                    // | true, value -> ()
                    // | _ -> ()

                    let loadProgress = 
                        (VStack() {
                            ProgressBar(0, model.LoadProgress.Max, model.LoadProgress.Current, fun _ -> Unit)
                                .horizontalAlignment(HorizontalAlignment.Stretch)

                            Button("Open...", LoadProgressMsg LoadProgress.OpenFile)
                                .margin(0, 2, 2, 0)
                                .horizontalAlignment(HorizontalAlignment.Left)
                        })

                    let content =
                        (Dock(true) {
                            loadProgress
                                .dock(Dock.Bottom)

                            match model.ViewModel with
                            | SpriteGridModel sgm ->
                                let sgm = { sgm with HighlightBrush = highlightBrush }

                                View.map SpriteGridMsg (SpriteGrid.view sgm)
                            | Empty -> ()
                        })
                            .margin(4)

                    let details =
                        (View.map (fun _ -> Unit) (SpriteDetailsPanel.view model.SelectedSprite))
                            .margin(4)
                        |> withAcrylic (acrylicMaterial panelAcrylicColor)
                    
                    SplitView(details, content)
                        .displayMode(SplitViewDisplayMode.Inline)
                        .isPaneOpen(true)
                        .panePlacement(SplitViewPanePlacement.Right)
                        .openPaneLength(450)
                        .paneBackground(Avalonia.Media.Colors.Transparent)

                })
                |> withAcrylic (acrylicMaterial acrylicColor)

            Window(view)
                .reference(model.Window)
                .transparencyLevelHint([WindowTransparencyLevel.AcrylicBlur])
                .background(Avalonia.Media.Colors.Transparent)

        let window =
            match icon with
            | Some icon ->
                window.icon(icon)
            | None -> window

        DesktopApplication(window)

    let init () =
        let spritesData = SpritesData.init()
        let windowRef = ViewRef<Window>()
        
        let model =
            {
                Sprites = spritesData
                ViewModel = SpriteGrid.init spritesData windowRef |> SpriteGridModel
                LoadProgress = LoadProgress.init windowRef
                Window = windowRef
            }
        
        model, Cmd.none

    let theme = FluentTheme()
    let program = Program.statefulWithCmd init update app
