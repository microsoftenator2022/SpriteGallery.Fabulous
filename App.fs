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
    type Model =
    | SpriteGridModel of SpriteGrid.Model
    | LoadProgressModel of BundleLoadView.LoadProgress
    
    type Msg =
    | Unit
    | LoadProgressMsg of BundleLoadView.LoadProgress.Msg
    | SpriteGridMsg of SpriteGrid.Msg

    // let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"
    let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\ui"

    let update (msg : Msg) (model : Model) =
        match msg, model with
        | SpriteGridMsg msg, SpriteGridModel (bm, _) ->
            match msg with
            | SpriteGrid.Msg.Unit -> model, Cmd.none
            | SpriteGrid.Msg.Resize size ->
                SpriteGridModel (bm, size), Cmd.none

        | LoadProgressMsg msg, LoadProgressModel model ->
            LoadProgressModel model, Cmd.none
        | _ -> model, Cmd.none

    let app (model : Model) =
        let window =
            let view =
                (Panel() {
                    match model with
                    | SpriteGridModel (bm, size) ->
                        View.map SpriteGridMsg (SpriteGrid.view (bm, size))
                    | LoadProgressModel lp ->
                        View.map LoadProgressMsg (BundleLoadView.view (lp))
                })

            Window(view)
                .onResized(fun args -> args.ClientSize |> SpriteGrid.Msg.Resize |> SpriteGridMsg)
        
        DesktopApplication(window)

    let init () =
        let bm = BaseModel.init filePath
        SpriteGridModel (SpriteGrid.Model (bm, Avalonia.Size())),
        // LoadProgressModel ({ Complete = false; Progress = (0, 100)}),

        Cmd.none

    let theme = FluentTheme()
    let program = Program.statefulWithCmd init update app
