type Customer = {
    Id : int
    IsVip : bool
    Credit : decimal
}

// Build a pipline of functions [Option]
// getPurchases >> tryPromoteToVip >> increaseCreditIfVip

// Customer -> Option<Customer * decimal>
let getPurchases customer =
    if customer.Id % 2 = 0 then Some (customer, 120M) 
    else None

// (Customer * decimal) -> Customer
let tryPromoteToVip customerPurchases =
    let (customer, amount) = customerPurchases
    if amount > 100M then { customer with IsVip = true }
    else customer

// Customer -> Option<Customer>
let increaseCreditIfVip customer =
    if customer.IsVip then Some { customer with Credit = customer.Credit + 100M }
    else None
    

let composeV1 customer =
    let result1 = getPurchases customer
    let result2 = Option.map tryPromoteToVip result1
    let result3 = Option.bind increaseCreditIfVip result2
    result3

// let composeV2 customer =
//     increaseCreditIfVip(tryPromoteToVip(getPurchases(customer)))

let composeV3 customer =
    customer
    |> getPurchases 
    |> Option.map tryPromoteToVip 
    |> Option.bind increaseCreditIfVip 

let composeV4 =
    getPurchases >> Option.map tryPromoteToVip >> Option.bind increaseCreditIfVip


let composeV3extended customer =
    customer
    |> getPurchases 
    |> fun result ->
        match result with
        | Some x -> Some (tryPromoteToVip x)
        | None -> None 
    |> fun result ->
        match result with
        | Some x -> increaseCreditIfVip x
        | None -> None






let vip = { Id = 1; IsVip = true; Credit = 200M }

let assertVip1 = (composeV1 vip = Some { vip with Credit = 300M })
// let assertVip2 = (composeV2 vip = { vip with Credit = 300M })
let assertVip3 = (composeV3 vip = Some { vip with Credit = 300M })
let assertVip4 = (composeV4 vip = Some { vip with Credit = 300M })

let promote = { Id = 2; IsVip = false; Credit = 120M } 

let assertPromote1 = (composeV1 promote = Some { promote with IsVip = true; Credit = 220M })
// let assertPromote2 = (composeV2 promote = { promote with IsVip = true; Credit = 220M })
let assertPromote3 = (composeV3 promote = Some { promote with IsVip = true; Credit = 220M })
let assertPromote4 = (composeV4 promote = Some { promote with IsVip = true; Credit = 220M })

let notPromote = { Id = 3; IsVip = false; Credit = 75M }

let assertNotPromote1 = (composeV1 notPromote = None)
// let assertNotPromote2 = (composeV2 notPromote = { notPromote with Credit = 125M })
let assertNotPromote3 = (composeV3 notPromote = None)
let assertNotPromote4 = (composeV4 notPromote = None)



