module SpriteGallery.Fabulous.Common

open Avalonia.Controls
open Avalonia.Media

open Fabulous
open Fabulous.Avalonia

open type Fabulous.Avalonia.View

let withAcrylic material content =
    ExperimentalAcrylicBorder(content)
        .material(material)

let acrylicMaterial color = 
    ExperimentalAcrylicMaterial()
        .materialOpacity(0.5)
        .tintOpacity(1)
        .backgroundSource(AcrylicBackgroundSource.Digger)
        .tintColor(color)
        .fallbackColor(color)

let tryGetThemeResource<'a> name (window : ViewRef<Window>) : 'a option =
    window.TryValue
    |> Option.bind (fun window ->
        match window.TryFindResource(name, window.ActualThemeVariant) with
        | true, color ->
            match color with
            | :? 'a as resource -> Some resource
            | _ ->
                eprintfn "Resource is not %A. Is %A" (typeof<'a>) (color.GetType())
                None
        | _ -> None
    )

let tryGetColor name (window : ViewRef<Window>) = tryGetThemeResource<Color> name window

let createBitmap bytes size =
    new Avalonia.Media.Imaging.Bitmap(
        Avalonia.Platform.PixelFormat.Bgra8888,
        Avalonia.Platform.AlphaFormat.Unpremul,
        System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(bytes, 0),
        size,
        Avalonia.Vector(96, 96),
        size.Width * 4)
        