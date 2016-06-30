module Parser

open CommonTypes
open FParsec.CharParsers
open FParsec.Primitives

let parseFen (fen : string) : Result<Position, string> =
    let player = (charReturn 'w' White) <|> (charReturn 'b' Black) 
    let playerBetweenSpaces = spaces1 >>. player .>> spaces1
    let pposition = many (skipAnyOf "pnbrqkPNBRQK12345678/") >>. playerBetweenSpaces
    let result = run pposition fen
    match result with
    | Success (result, _, _) -> Result.Success { PlayerInTurn = result } 
    | Failure (errorMessage, _, _) -> Result.Failure errorMessage 
