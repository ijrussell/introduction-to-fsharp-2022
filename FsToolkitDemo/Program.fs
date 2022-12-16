// Login requires the following functions:
//
//   tryGetUser: string -> Task<Option<User>>
//   isPwdValid: string -> User -> bool
//   authorize: User -> Task<Result<unit, AuthError>>
//   createAuthToken: User -> Result<AuthToken, TokenError>

open System
open FsToolkit.ErrorHandling
open System.Threading.Tasks

type AuthToken = { Value : Guid }
type AuthError = AuthError of string
type TokenError = TokenError of string

type LoginError = 
    | InvalidUser of string 
    | InvalidPwd 
    | Unauthorized of AuthError 
    | NoToken of TokenError

type User = {
    Name : string
    Password : string
}

[<Literal>]
let InvalidUserName = "12345"
[<Literal>]
let CorrectPassword = "mypassword"

let tryGetUser (userName:string) =
    task {
        return 
            match userName with
            | InvalidUserName -> None
            | name -> Some { Name = name; Password = CorrectPassword }
    }

let isPwdValid (password:string) (user:User) =
    password = user.Password

let authorize (user:User) =
    task {
        return 
            if user.Name <> "authorize" then Ok () else 
            Error (AuthError $"User: {user.Name} is not authorised")
    }

let createAuthToken (user:User) =
    if user.Name <> "authtoken" then Ok { Value = Guid.NewGuid() } 
    else Error (TokenError $"Cannot create authtoken for User: {user.Name}")

let login (userName: string) (password: string) : Task<Result<AuthToken, LoginError>> =
    taskResult {
        let! user = userName |> tryGetUser |> TaskResult.requireSome (InvalidUser userName)
        do! user |> isPwdValid password |> Result.requireTrue InvalidPwd
        do! user |> authorize |> TaskResult.mapError Unauthorized
        return! user |> createAuthToken |> Result.mapError NoToken
    }

// var async AuthToken Login(String userName, String password) {
//     var user = await TryGetUser(userName);
//     if (user == null) {
//         throw new InvalidUserException $"User: {userName}";
//     }
//     if !(IsPasswordValid(password, userName)) {
//         throw new InvalidPasswordException $"User: {user.Name}";;  
//     }
//     if !(await Authorize(user)) {
//         throw new UnauthorizedException $"{user.Name} is not authorised");     
//     }
//     var authToken = CreateAuthToken(user);
//     if (authToken == null) {
//         throw new AuthTokenException $"Cannot create authtoken for {user.Name}";  
//     }
//     return authToken;
// }

[<EntryPoint>]
let main argv =
    [
        ("ijrussell", CorrectPassword) // Ok
        (InvalidUserName, "") // Error InvalidUser
        ("ijrussell", "password") // Error InvalidPassword
        ("authorize", CorrectPassword) // Error Unauthorized
        ("authtoken", CorrectPassword) // Error TokenErr
    ]
    |> List.iter (fun (username, password) ->
        login username password
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> printfn "%A")
    0 // return an integer exit code