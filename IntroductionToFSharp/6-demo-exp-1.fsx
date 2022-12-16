// Computation Expressions - 1

let multiply (x:int) (y:int) = // int -> int -> int
    x * y

let divide (x:int) (y:int) = // int -> int -> int option
    if y = 0 then None
    else Some (x / y)

// The formula is: f x y = ((x / y) * x) / y
let calculate x y =
    divide x y
    |> fun v -> multiply v x // compiler error
    |> fun t -> divide t y

let calculate1 x y =
    divide x y
    |> fun result -> 
        match result with
        | Some v -> Some (multiply v x)
        | None -> None
    |> fun result -> 
        match result with
        | Some t -> divide t y
        | None -> None

let calculate2 x y =
    divide x y
    |> Option.map (fun v -> multiply v x)
    |> Option.bind (fun t -> divide t y)

// Warning - It looks like magic but it isn't!

[<AutoOpen>]
module Option =
    type OptionBuilder() =
    // Supports let!
        member _.Bind(x, f) = Option.bind f x
        // Supports return
        member _.Return(x) = Some x
        // Supports return!
        member _.ReturnFrom(x) = x

// Computation Expression for Option
// Usage will be option {...}
let option = OptionBuilder()


let calculate3 x y =
    option {
        let! v = divide x y
        let t = multiply v x
        let! r = divide t y
        return r
    }

let good = calculate3 10 5

let bad = calculate3 10 0

