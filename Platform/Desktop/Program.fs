namespace SpriteGallery.Fabulous.Desktop

open System
open Avalonia
open Fabulous.Avalonia
open SpriteGallery.Fabulous

module Program =

    [<CompiledName "BuildAvaloniaApp">]
    let buildAvaloniaApp () =
        AppBuilder
            .Configure(fun () ->
                let app = Program.startApplication App.program
                app.Styles.Add(App.theme)
                app)
            .LogToTrace(areas = Array.empty)
            .UsePlatformDetect()

    [<EntryPoint; STAThread>]
    let main argv =
        buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
