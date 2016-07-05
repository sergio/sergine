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

// Sub-parsers

let whitePlayer = charReturn 'w' White
let blackPlayer = charReturn 'b' Black

let pturn : Parser<Player, unit> = whitePlayer <|> blackPlayer  

let pCastlings : Parser<AvailableCastling list, unit> =
    (charReturn '-' [] 
    <|> many (
            charReturn 'K' { Player = White; Side = Kingside }
        <|> charReturn 'Q' { Player = White; Side = Queenside }
        <|> charReturn 'k' { Player = Black; Side = Kingside }
        <|> charReturn 'q' { Player = Black; Side = Queenside }
        )
    )

let pRankNumber : Parser<int, unit> =
    anyOf "12345678" |>> (fun c -> int(c) - int('1'))

let pFileLetter : Parser<int, unit> =
    anyOf "abcdefgh" |>> (fun c -> int(c) - int('a'))

let pSquare : Parser<Coordinate, unit> =
    pFileLetter .>>. pRankNumber

let pEnPassantSquare : Parser<Coordinate option, unit> =
    (charReturn '-' None) <|> (pSquare |>> Some)

// Sub-parser testing functions

let parseTurn (s : string) : Result<Player, string> = 
    let r = run (pturn .>> eof) s
    match r with
    | Success (player, _, _) -> Result.Success player
    | Failure (msg, _, _) -> Result.Failure msg

let parseCastlings (s: string) : Result<AvailableCastling list, string> =
    let r = run (pCastlings .>> eof) s
    match r with
    | Success (castlings, _, _) -> Result.Success castlings
    | Failure (msg, _, _) -> Result.Failure msg

let parseEnPassantTarget (s: string) : Result<Coordinate option, string> =
    let r = run (pEnPassantSquare .>> eof) s
    match r with
    | Success (player, _, _) -> Result.Success player
    | Failure (msg, _, _) -> Result.Failure msg
    