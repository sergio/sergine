module Parser

open CommonTypes
open FParsec.CharParsers
open FParsec.Primitives

let parseFen (fen : string) : Result<Position, string> =
    let playerBetweenSpaces = spaces1 >>. (pchar 'w' <|> pchar 'b') .>> spaces1
    let pposition = many (skipAnyOf "pnbrqkPNBRQK12345678/") >>. playerBetweenSpaces
    let result = run pposition fen
    match result with
    | Success (result, _, _) -> 
        match result with
        | 'w' -> Result.Success { PlayerInTurn = White }
        | _ -> Result.Success { PlayerInTurn = Black }
    | Failure (errorMessage, _, _) -> Result.Failure errorMessage 
