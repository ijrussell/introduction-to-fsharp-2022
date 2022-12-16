// Computation Expressions - 2

let multiply (x:int) (y:int) = 
    x * y

let divide (x:int) (y:int) = 
    try
        Ok (x / y)
    with 
    | ex -> Error ex

// The formula is: f x y = ((x / y) * x) / y
let calculate x y =
    divide x y
    |> fun v -> multiply v x
    |> fun t -> divide t y

let calculate1 x y =
    divide x y
    |> fun result -> 
        match result with
        | Ok v -> Ok (multiply v x)
        | Error ex -> Error ex
    |> fun result -> 
        match result with
        | Ok t -> divide t y
        | Error ex -> Error ex




