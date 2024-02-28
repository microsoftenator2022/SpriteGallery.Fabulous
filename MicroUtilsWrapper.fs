module SpriteGallery.Fabulous.MicroUtils

open MicroUtils

let toOption (microOption : Functional.Option<_>) = if microOption.IsSome then Some microOption.Value else None
let toMicroOption = function Some value -> Functional.Option<_>.Some(value) | None -> Functional.Option<_>.None
