module Parser

open CommonTypes
open FParsec.CharParsers
open FParsec.Primitives

// let parseFen (fen : string) : Result<Position, string> =
//     let player = (charReturn 'w' White) <|> (charReturn 'b' Black) 
//     let playerBetweenSpaces = spaces1 >>. player .>> spaces1
//     let pposition = many (skipAnyOf "pnbrqkPNBRQK12345678/") >>. playerBetweenSpaces
//     let result = run pposition fen
//     match result with
//     | Success (result, _, _) -> Result.Success { Turn = result } 
//     | Failure (errorMessage, _, _) -> Result.Failure errorMessage 

let whitePlayer = charReturn 'w' White
let blackPlayer = charReturn 'b' Black

let pturn : Parser<Player, unit> = whitePlayer <|> blackPlayer  

let parseTurn (s : string) : Result<Player, string> = 
    let r = run pturn s
    match r with
    | Success (player, _, _) -> Result.Success player
    | Failure (msg, _, _) -> Result.Failure msg

