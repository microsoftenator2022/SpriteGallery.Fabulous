module MicroUtils.Interop

open MicroUtils

type MicroOption<'a> = Functional.Option<'a>

#nowarn "3391"
let toOption (value : MicroOption<'a>) : Option<'a> = value
// let toMicroOption (value : Option<'a>) : MicroOption<'a> = value

// let toOption (microOption : Functional.Option<_>) = if microOption.IsSome then Some microOption.Value else None
// let toMicroOption = function Some value -> Functional.Option<_>.Some(value) | None -> Functional.Option<_>.None
