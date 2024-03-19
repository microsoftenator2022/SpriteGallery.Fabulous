[<RequireQualifiedAccess>]
module SpriteGallery.Fabulous.Views.LoadProgress

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Platform

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

open SpriteGallery.Fabulous
open SpriteGallery.Fabulous.Common

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

let view (model : Model) =
    (VStack() {
        ProgressBar(0, model.Max, model.Current, fun _ -> Unit)
            .horizontalAlignment(HorizontalAlignment.Stretch)

        Button("Open...", OpenFile)
            .margin(0, 2, 2, 0)
            .horizontalAlignment(HorizontalAlignment.Left)
    })
