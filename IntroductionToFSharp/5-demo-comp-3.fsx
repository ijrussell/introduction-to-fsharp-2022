type Customer = {
    Id : int
    IsVip : bool
    Credit : decimal
}

// Build a pipline of functions [Result]
// getPurchases >> tryPromoteToVip >> increaseCreditIfVip

// Customer -> Result<(Customer * decimal),string>
let getPurchases customer =
    if customer.Id % 2 = 0 then Ok (customer, 120M)
    else Error "Customer is bad news"
    

// (Customer * decimal) -> Customer
let tryPromoteToVip customerPurchases =
    let (customer, amount) = customerPurchases
    if amount > 100M then { customer with IsVip = true }
    else customer

// Customer -> Result<Customer,string>
let increaseCreditIfVip customer =
    if customer.IsVip then Ok { customer with Credit = customer.Credit + 100M }
    else Error "Unable to increase credit"
    

let composeV1 customer =
    let result1 = getPurchases customer
    let result2 = Result.map tryPromoteToVip result1
    let result3 = Result.bind increaseCreditIfVip result2
    result3

// let composeV2 customer =
//     increaseCreditIfVip(tryPromoteToVip(getPurchases(customer)))

let composeV3 customer =
    customer
    |> getPurchases 
    |> Result.map tryPromoteToVip 
    |> Result.bind increaseCreditIfVip 

let composeV4 =
    getPurchases >> Result.map tryPromoteToVip >> Result.bind increaseCreditIfVip


let composeV5 customer =
    customer
    |> getPurchases 
    |> fun result ->
        match result with
        | Ok data -> Ok (tryPromoteToVip data)
        | Error ex -> Error ex
    |> fun result ->
        match result with
        | Ok data -> increaseCreditIfVip data
        | Error ex -> Error ex





