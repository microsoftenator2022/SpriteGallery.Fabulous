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

open System.Threading
open MicroUtils.Interop

type LoadContext (filePath : string, loadDependencies, loadBlueprintAssets) =
    let dir = System.IO.Path.GetDirectoryName filePath
    let baPath = System.IO.Path.Join(dir, "blueprint.assets")

    let mutable started = false
    let mutable spriteCount = 0
    let mutable complete = false
    let mutable spritesData = { Sprites = []; Textures = Map.empty }

    let newWaitHandle() = new EventWaitHandle(false, EventResetMode.AutoReset)
    let mutable waitHandle = lazy(newWaitHandle())
    let spriteUpdate = Event<Sprite option>()

    let loadProc() =
        started <- true

        AssetLoader.init()

        let archive =
            if loadDependencies then
                AssetLoader.mountArchiveWithDependencies filePath |> fst
            else AssetLoader.mountArchive filePath

        if loadBlueprintAssets then    
            AssetLoader.mountArchive baPath |> ignore
        
        let spriteObjectInfos = AssetLoader.getSpriteObjectsInArchive archive

        spriteCount <- spriteObjectInfos.Length

        for (sfPath, o) in spriteObjectInfos do
            waitHandle.Force().WaitOne() |> ignore
            
            let sf = AssetLoader.getSerializedFile sfPath |> toOption
            let sprite = sf |> Option.bind (fun sf -> AssetLoader.getSprite o sf)

            spritesData <-
                let spritesData = { spritesData with Textures = AssetLoader.textures }

                match sprite with
                | Some sprite -> { spritesData with Sprites = sprite :: spritesData.Sprites }
                | None -> spritesData

            spriteUpdate.Trigger sprite

        AssetLoader.cleanup()

        complete <- true

        spriteUpdate.Trigger None

    let thread =
        let thread = ThreadStart(loadProc) |> Thread
        thread.IsBackground <- true
        thread

    member _.Count = spriteCount
    member _.SpritesData = spritesData
    member _.Complete = complete

    [<CLIEvent>]
    member private _.spriteUpdateEvent = spriteUpdate.Publish

    member _.Start() =
        thread.Start()

    member this.GetOneAsync() = async {
        waitHandle.Force().Set() |> ignore

        return! this.spriteUpdateEvent |> Async.AwaitEvent
    }

    interface System.IDisposable with
        member _.Dispose() =
            if waitHandle.IsValueCreated then
                waitHandle.Value.Dispose()
                waitHandle <- lazy(newWaitHandle())

type LoadProgress = { Max : int; Current : int; SpritesData : SpritesData }

type State =
| SelectBundle
| LoadSprites of LoadProgress * LoadContext
| GenerateThumbnails of LoadProgress

type Model =
  { LoadDependencies : bool
    LoadBlueprintAssets : bool
    FilePath : string
    State : State
    SpritesData : SpritesData
    Window : ViewRef<Window>
    StatusMessage : string }

let init window =
    {
        LoadDependencies = true
        LoadBlueprintAssets = true
        FilePath = ""
        State = SelectBundle
        SpritesData = SpritesData.init()
        Window = window
        StatusMessage = ""
    }

type Msg =
| Unit
| OpenFile
| UpdatePath of string
| Start
| ProgressUpdate of State
| ToggleLoadDependencies of bool
| ToggleLoadBlueprintAssets of bool
| StatusMessage of string
| Complete

let loadStep = function
| LoadSprites (progress, context) when not context.Complete ->
    async {
        let! sprite = context.GetOneAsync()
        
        let progress =
            { progress with
                Current = progress.Current + 1
                Max = context.Count
                SpritesData = context.SpritesData
            }

        return LoadSprites (progress, context) |> ProgressUpdate |> Some
    }
    |> Cmd.ofAsyncMsgOption
| LoadSprites (progress, context) ->
    (context :> System.IDisposable).Dispose()
    
    [
        StatusMessage "Generating thumbnails"
        |> Cmd.ofMsg

        GenerateThumbnails { progress with Current = 0 }
        |> ProgressUpdate
        |> Cmd.ofMsg
    ] |> Cmd.batch

| GenerateThumbnails progress ->
    if progress.Current < progress.Max then
        async {
            let tasks = seq {
                for i in 0..(System.Environment.ProcessorCount - 1) do
                // for i in 0..0 do
                    let i = progress.Current + i

                    if i < progress.Max then
                        let task =
                            System.Threading.Tasks.Task.Run(fun () ->
                                progress.SpritesData.Sprites[i].GetHeightScaledBitmap(tileSize) |> ignore
                                i)
                            |> Async.AwaitTask

                        yield task
            }

            let! results = tasks |> Async.Parallel

            return GenerateThumbnails { progress with Current = (results |> Seq.max) + 1 } |> ProgressUpdate
        }
        // Tasks.Task.Run(fun () ->
        //     let _ = progress.SpritesData.Sprites[progress.Current].GetHeightScaledBitmap(SpriteGrid.tileSize)
            
        //     GenerateThumbnail { progress with Current = progress.Current + 1 }
        //     |> ProgressUpdate)
        // |> Async.AwaitTask
        |> Cmd.ofAsyncMsg
    else
        [
            "Done"
            |> StatusMessage
            |> Cmd.ofMsg

            Complete
            |> Cmd.ofMsg
        ] |> Cmd.batch
| SelectBundle -> Cmd.none

let loadStart model =
    let dir = System.IO.Path.GetDirectoryName model.FilePath
    let baPath = System.IO.Path.Join(dir, "blueprint.assets")

    if System.IO.File.Exists model.FilePath |> not then
        { model with StatusMessage = sprintf "'%s' does not exist" model.FilePath }, Cmd.none
    elif model.LoadBlueprintAssets && System.IO.File.Exists baPath |> not then
        { model with StatusMessage = sprintf "'%s' does not exist" baPath }, Cmd.none
    else
        model,
        [
            StatusMessage "Loading sprites"
            |> Cmd.ofMsg

            (
                let context = new LoadContext(model.FilePath, model.LoadDependencies, model.LoadBlueprintAssets)
                context.Start()

                LoadSprites ({ Current = 0; Max = 1; SpritesData = SpritesData.init() }, context)
                |> ProgressUpdate
                |> Cmd.ofMsg
            )
        ] |> Cmd.batch

let update msg model =
    match model.State, msg with
    | _, StatusMessage message -> { model with StatusMessage = message }, Cmd.none
    | SelectBundle, OpenFile ->
        match model.Window.TryValue with
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
                return UpdatePath path |> Some
            | None -> return None
            })

    | SelectBundle, UpdatePath path -> { model with FilePath = path }, Cmd.none
    | SelectBundle, ToggleLoadDependencies state -> { model with LoadDependencies = state }, Cmd.none
    | SelectBundle, ToggleLoadBlueprintAssets state -> { model with LoadBlueprintAssets = state }, Cmd.none
    | SelectBundle, Start -> loadStart model
    | _, ProgressUpdate update ->
        let model =
            { model with
                State = update
                SpritesData =
                    match update with
                    | SelectBundle -> model.SpritesData
                    | LoadSprites (progress, _) -> progress.SpritesData
                    | GenerateThumbnails progress -> progress.SpritesData }

        model, loadStep update
    | _ ->
        eprintfn "WARNING: Unhandled LoadProgress message %A" msg
        model, Cmd.none

let view model =
    let (progress, total) =
        match model.State with
        | SelectBundle -> (0.0, 1.0)
        | LoadSprites (progress, _) -> (progress.Current, progress.Max * 2 |> float)
        | GenerateThumbnails progress -> (progress.Max + progress.Current |> float, progress.Max * 2 |> float)

    // let statusMessage =
    //     match model.State with
    //     | SelectBundle -> ""
    //     | LoadSprites _ -> "Loading sprites"
    //     | GenerateGidLayout _ -> "Creating thumbnails"

    (Grid(coldefs = [Star; Stars 6; Star], rowdefs = [Star; Stars 6; Star]) {
        (VStack() {
            match tryGetSuspectingIcon() with
            | Some icon ->
                Image(icon)
                    .size(icon.Size.Width, icon.Size.Height)
                    .margin(4)
            | None -> ()

            (Grid(coldefs = [Star; Auto], rowdefs = [Auto]) {
                TextBox(model.FilePath, fun text -> UpdatePath text)
                    .isEnabled(model.State = SelectBundle)
                    .gridRow(0)
                    .gridColumn(0)
                
                Button("Select...", OpenFile)
                    .gridRow(0)
                    .gridColumn(1)
            })
                .horizontalAlignment(HorizontalAlignment.Stretch)

            CheckBox("Load bundle dependencies", model.LoadDependencies, fun s -> ToggleLoadDependencies s)
                .horizontalAlignment(HorizontalAlignment.Left)
                .isEnabled(model.State = SelectBundle)

            CheckBox("Load blueprint.assets", model.LoadBlueprintAssets, fun s -> ToggleLoadBlueprintAssets s)
                .horizontalAlignment(HorizontalAlignment.Left)
                .isEnabled(model.State = SelectBundle)

            Button("Open", Start)
                .isEnabled(model.State = SelectBundle && model.FilePath <> "")

            ProgressBar(0, total, progress, fun _ -> Unit)
                .horizontalAlignment(HorizontalAlignment.Stretch)

            TextBlock(model.StatusMessage)
                .centerHorizontal()
        })
            .centerVertical()
            .gridRow(1)
            .gridColumn(1)
    })
