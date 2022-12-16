// Reducing primitives

open System
open System.Text.RegularExpressions

let doSomething (customerId:Guid) (orderId:Guid) =
    ()

// Single-case discriminated union

type CustomerId = CustomerId of Guid
type OrderId = OrderId of Guid

let customerId = CustomerId (Guid.NewGuid())
let orderId = CustomerId (Guid.NewGuid())

let result = doSomething customerId orderId

let doSomething1 (customerId:CustomerId) (orderId:OrderId) =
    ()

let result = doSomething1 customerId orderId

let customerId = CustomerId (Guid.NewGuid())
let orderId = OrderId (Guid.NewGuid())

let result = doSomething1 customerId orderId

let doSomething2 (CustomerId customerId) (OrderId orderId) =
    (customerId, orderId)

// Deconstruct

let (CustomerId id) = customerId


// Single-case discriminated union with logic

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

let (|IsValidEmail|_|) input =
    match input with
    | ParseRegex ".*?@(.*)" [ _ ] -> Some input
    | _ -> None


type Email = private Email of string

module Email =

    let value input = input |> fun (Email value) -> value

    let create input =
        match input with
        | IsValidEmail _ -> Ok (Email input)
        | _ -> Error $"{input} is not a valid email"


let wrapped = Email.create "test@hello.org" 

let unwrapIfValid input = 
    match input with
    | Ok x -> Email.value x |> Some
    | _ -> None

