namespace SpriteGallery.Fabulous

open Avalonia.Platform
open Avalonia.Media.Imaging
open Avalonia.Themes.Fluent

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open UnityDataTools.FileSystem

open SpriteGallery.Fabulous.Views

module App =
    type ViewModel =
    | Empty
    | SpriteGridModel of SpriteGrid.Model

    type LoadProgress = { Complete : bool; Current : int; Max : int }
        with static member Default = { Complete = false; Current = 0; Max = 100 }

    type Model =
      { Sprites : SpritesData
        ViewModel : ViewModel
        LoadProgress : LoadProgress
        WindowRef : ViewRef<Avalonia.Controls.Window> }
    
    type Msg =
    | Unit
    | OpenFile
    | UpdateSprites of SpritesData
    | LoadProgressMsg of SpriteGetter
    | SpriteGridMsg of SpriteGrid.Msg

    // let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"
    // let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

    let update (msg : Msg) (model : Model) =
        match msg, model.ViewModel with
        | SpriteGridMsg msg, SpriteGridModel sgm ->
            let spritesData, sgm = SpriteGrid.update msg (model.Sprites, sgm)

            { model with Sprites = spritesData; ViewModel = SpriteGridModel sgm }, Cmd.none
        | OpenFile, _ ->
            model.WindowRef.TryValue
            |> function
            | None ->
                eprintfn "no windowref"
                model, Cmd.none
            | Some window ->
                model,
                Cmd.ofAsyncMsgOption (async
                {
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
                        
                        return Some (LoadProgressMsg sg)
                    | None -> return None
                })
        | UpdateSprites spritesData, _ -> { model with Sprites = spritesData }, Cmd.none
        | LoadProgressMsg getter, _ ->
            let (current, max) = getter.Progress

            let model = { model with LoadProgress = { Complete = getter.Complete; Current = current; Max = max; } }

            if getter.Complete then
                printfn "Done"
                let sprites = SpritesData.getResult getter

                (getter :> System.IDisposable).Dispose()

                model, Cmd.ofMsg(UpdateSprites sprites)
            else
                model, Cmd.ofAsyncMsg (async {
                    let! _ = 
                        Async.AwaitEvent getter.Update
                        // |> Async.Ignore

                    return (LoadProgressMsg getter)
                })
        | Unit, _ -> model, Cmd.none
        | _, Empty -> model, Cmd.none

    let app (model : Model) =
        let window =
            let view =
                (Dock(true) {
                    (Dock(true) {
                        Button("Open...", OpenFile)
                            .dock(Avalonia.Controls.Dock.Bottom)
                            .margin(0, 2, 2, 0)

                        ProgressBar(0, model.LoadProgress.Max, model.LoadProgress.Current, fun _ -> Unit)
                            .dock(Avalonia.Controls.Dock.Top)
                    })
                        .dock(Avalonia.Controls.Dock.Bottom)
                        .margin(2)
                    
                    (Panel() {
                        match model.ViewModel with
                        | SpriteGridModel size ->
                            View.map SpriteGridMsg (SpriteGrid.view (model.Sprites, size))
                        | Empty -> ()
                    })
                        .margin(1)
                })

            Window(view)
                .onResized(fun args -> args.ClientSize |> SpriteGrid.Msg.Resize |> SpriteGridMsg)
                .reference(model.WindowRef)
                .transparencyLevelHint([Avalonia.Controls.WindowTransparencyLevel.AcrylicBlur])
                .background(Avalonia.Media.Colors.Transparent)

        DesktopApplication(window)

    let init () =
        let spritesData, sgm = SpritesData.init() |> SpriteGrid.init

        { 
            Sprites = spritesData
            ViewModel = sgm |> SpriteGridModel
            LoadProgress = LoadProgress.Default
            WindowRef = ViewRef<Avalonia.Controls.Window>()
        }, Cmd.none

        // LoadProgressModel ({ Complete = false; Progress = (0, 100)}),

    let theme = FluentTheme()
    let program = Program.statefulWithCmd init update app
