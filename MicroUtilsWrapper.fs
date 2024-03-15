module MicroUtils.Interop

open MicroUtils

type MicroOption<'a> = Functional.Option<'a>

#nowarn "3391"
let toOption (value : MicroOption<'a>) : Option<'a> = value
// let toMicroOption (value : Option<'a>) : MicroOption<'a> = value

// let toOption (microOption : Functional.Option<_>) = if microOption.IsSome then Some microOption.Value else None
// let toMicroOption = function Some value -> Functional.Option<_>.Some(value) | None -> Functional.Option<_>.None

[<RequireQualifiedAccess>]
module TypeTreeObject =
    open MicroUtils.UnityFilesystem
    let tryGetField<'a> fieldName (tto : ITypeTreeObject) : 'a option =
        tto.TryGetField<'a>(fieldName)
        |> toOption
        |> Option.map (fun f -> f.Invoke())

    let toMap (tto : ITypeTreeObject) =
        tto.ToDictionary()
        |> Seq.map (fun pair -> pair.Key, pair.Value)
        |> Map.ofSeq
