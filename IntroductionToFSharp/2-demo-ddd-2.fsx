(*
Feature: Applying a discount

Scenario: Eligible Registered Customers get 10% discount 
when they spend £100 or more

Given the following Registered Customers
|Customer Id|Email        |Is Eligible|
|John       |john@test.org|true       |
|Mary       |mary@test.org|true       |
|Richard    |             |false      |

When <Customer Id> spends <Spend>
Then their order total will be <Total>

Examples:
|Customer Id| Spend | Total |
|Mary       |  99.00|  99.00|
|John       | 100.00|  90.00|
|Richard    | 100.00| 100.00|
|Sarah      | 100.00| 100.00|
*)

// Discriminated Union (OR)
type CustomerStatus =
    | Registered of IsEligible:bool * Email:Option<string> 
    | Guest

// Record type (AND)
type Customer = {
    Name: string
    Status: CustomerStatus
}

let calculateOrderTotal (customer: Customer) (spend: decimal) =
    let discount = 
        match customer.Status with
        | Registered (IsEligible = true) when spend >= 100M -> spend * 0.1M
        | Registered _ -> 0M
        | Guest -> 0M
    spend - discount

let john: Customer = { Name = "John"; Status = Registered (IsEligible = true, Email = Some "john@test.org") }
let mary = { john with Name = "Mary"; Status = Registered (IsEligible = true, Email = Some "mary@test.org") }
let richard = { john with Name = "Richard"; Status = Registered (IsEligible = false, Email = None) }
let sarah = { richard with Name = "Sarah"; Status = Guest }

let assertJohn = calculateOrderTotal john 100.0M = 90.0M
let assertMary = calculateOrderTotal mary 99.0M = 99.0M
let assertRichard = calculateOrderTotal richard 100.0M = 100.0M
let assertSarah = calculateOrderTotal sarah 100.0M = 100.0M

// How well does our design match our requirements?
