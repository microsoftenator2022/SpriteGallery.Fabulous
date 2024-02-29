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
    let filePath = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"

    let update (msg : Msg) model =
        model, Cmd.none

    let app model =
        DesktopApplication(Window(SpriteGrid.view model))

    let theme = FluentTheme()

    let program = Program.statefulWithCmd (fun () -> Model.init filePath, Cmd.none) update app
