namespace SpriteGallery.Fabulous

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Platform
open Avalonia.Media.Imaging
open Avalonia.Themes.Fluent

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous.Views

[<RequireQualifiedAccess>]
module LoadProgress =
    type Model = { Complete : bool; Current : int; Max : int; Window : ViewRef<Window>; Sprites : SpritesData option }

    let init() = { Complete = false; Current = 0; Max = 1; Window = ViewRef<Window>(); Sprites = None }

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

            let model = { model with Complete = getter.Complete; Current = current; Max = max; }

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
        LoadProgress : LoadProgress.Model }
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

    // let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"
    // let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

    let update (msg : Msg) (model : Model) =
        match msg, model.ViewModel with
        | SpriteGridMsg sgMsg, SpriteGridModel sgModel ->
            let sgModel, cmd = SpriteGrid.update sgMsg sgModel

            { model with ViewModel = SpriteGridModel sgModel }, Cmd.map SpriteGridMsg cmd

        | LoadProgressMsg lpMsg, _ ->
            let lpModel, cmd = LoadProgress.update lpMsg model.LoadProgress
            let model = { model with LoadProgress = lpModel }
            
            match lpModel.Sprites with
            | Some spritesData ->
                let model = { model with Sprites = spritesData }

                model, Cmd.batch [Cmd.map LoadProgressMsg cmd; Cmd.ofMsg UpdateSprites]
            | None -> model, Cmd.map LoadProgressMsg cmd

        | UpdateSprites, viewModel ->
            match viewModel with
            | Empty -> model, Cmd.none
            | SpriteGridModel _ ->
                model, Cmd.map SpriteGridMsg (model.Sprites |> SpriteGrid.UpdateSprites |> Cmd.ofMsg)

        | Unit, _ -> model, Cmd.none
        | _, Empty -> model, Cmd.none

    let app (model : Model) =
        let window =
            let view =
                (Dock(true) {
                    (Dock(true) {
                        Button("Open...", LoadProgressMsg LoadProgress.OpenFile)
                            .dock(Dock.Bottom)
                            .margin(0, 2, 2, 0)

                        ProgressBar(0, model.LoadProgress.Max, model.LoadProgress.Current, fun _ -> Unit)
                            .dock(Dock.Top)
                    })
                        .dock(Dock.Bottom)
                        .margin(2)
                    
                    let content =
                        (Panel() {
                            match model.ViewModel with
                            | SpriteGridModel sgm ->
                                View.map SpriteGridMsg (SpriteGrid.view sgm)
                            | Empty -> ()
                        })

                    SplitView(
                        (View.map (fun _ -> Unit) (SpriteDetailsPanel.view model.SelectedSprite)), content)
                            .displayMode(SplitViewDisplayMode.Inline)
                            .isPaneOpen(true)
                            .panePlacement(SplitViewPanePlacement.Right)
                            .openPaneLength(450)
                            .margin(1)
                })

            Window(view)
                .reference(model.LoadProgress.Window)
                .transparencyLevelHint([WindowTransparencyLevel.AcrylicBlur])
                .background(Avalonia.Media.Colors.Transparent)

        DesktopApplication(window)

    let init () =
        let spritesData = SpritesData.init()

        { 
            Sprites = spritesData
            ViewModel = SpriteGrid.init spritesData |> SpriteGridModel
            LoadProgress = LoadProgress.init()
        }, Cmd.none

    let theme = FluentTheme()
    let program = Program.statefulWithCmd init update app
