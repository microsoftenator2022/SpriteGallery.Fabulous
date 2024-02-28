namespace SpriteGallery.Fabulous

open Avalonia.Platform
open Avalonia.Media.Imaging
open Avalonia.Themes.Fluent

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

module App =
    open UnityDataTools.FileSystem
    type Model =
        {
            Bitmaps : (Bitmap * Avalonia.PixelRect * string) list
        }
    type Msg = unit

    let init () =
        let archiveFile = @"D:\SteamLibrary\steamapps\common\Warhammer 40,000 Rogue Trader\Bundles\blueprint.assets"

        UnityFileSystem.Init()

        let bundlesDirectory = System.IO.Path.GetDirectoryName(archiveFile)

        let dependencylist =
            System.IO.Path.Join(bundlesDirectory, "dependencylist.json")

        let dependencies =
            if System.IO.File.Exists dependencylist then
                UnityData.getDependencies dependencylist
                |> Map.tryFind (System.IO.Path.GetFileName(archiveFile))
                |> Option.map (fun files ->
                    seq {
                        for f in files do
                            let path = System.IO.Path.Join(bundlesDirectory, f)
                            UnityFileSystem.MountArchive(path, UnityData.mountPoint)
                    }
                    |> Seq.toArray)
                |> function
                | Some list -> list
                | None -> [||]
            else [||]

        let archive = UnityFileSystem.MountArchive(archiveFile, UnityData.mountPoint)

        let sfNode =
            archive.Nodes
            |> Seq.find (fun n -> n.Flags.HasFlag(ArchiveNodeFlags.SerializedFile))

        let sfPath = $"{UnityData.mountPoint}{sfNode.Path}"

        let bitmaps =
            UnityData.getSprites dependencies sfPath
            |> Seq.choose (
                function
                | Some sprite, name ->
                    sprite.TextureRect
                    |> Option.map (fun rect ->
                        (sprite.Bitmap, rect, name |> Option.defaultValue "" |> sprintf "%A")
                    )
                | _ -> None
            )

        { Bitmaps = bitmaps |> Seq.sortBy (fun (_, _, name) -> name) |> Seq.toList }

    let update (msg : Msg) model =
        model, Cmd.none

    let view model =
        (ScrollViewer(
            (VStack() {
                for (bitmap, rect, name) in model.Bitmaps do
                    (VStack() {
                        TextBlock($"{name}").centerText()

                        Image(Avalonia.Media.Stretch.None, CroppedBitmap(bitmap, rect)).renderTransform(ScaleTransform(1, -1))
                    }).centerHorizontal()
            }).centerHorizontal()
        ))

    let app model =
        DesktopApplication(Window(view model))

    let theme = FluentTheme()

    let program = Program.statefulWithCmd (fun () -> init(), Cmd.none) update app
