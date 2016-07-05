module Parser

open System
open CommonTypes
open FParsec
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

let ppiece : Parser<Piece, unit> =
    let a p k = { Player = p; Kind = k } 
    charReturn 'p' (a Black Pawn) 
<|> charReturn 'n' (a Black Knight)
<|> charReturn 'b' (a Black Bishop)
<|> charReturn 'r' (a Black Rook)
<|> charReturn 'q' (a Black Queen)
<|> charReturn 'k' (a Black King)
<|> charReturn 'P' (a White Pawn) 
<|> charReturn 'N' (a White Knight)
<|> charReturn 'B' (a White Bishop)
<|> charReturn 'R' (a White Rook)
<|> charReturn 'Q' (a White Queen)
<|> charReturn 'K' (a White King)

let pempties : Parser<Piece option list, unit> =
    anyOf "12345678" |>> (fun c -> c |> string |> Int32.Parse |> List.replicate <| None )

let ppieces : Parser<Piece list, unit> = many1 ppiece

let pRank : Parser<Piece option list, unit> =
    let pPiecesAndEmpties = many1 ((ppieces |>> (List.map Some)) <|> pempties) |>> List.concat
    fun stream ->
        let reply = pPiecesAndEmpties stream
        if reply.Status = Ok then
            if reply.Result.Length = 8 then
                reply
            else
                Reply(FatalError, messageError ("There must be exactly 8 squares in a rank")) 
        else
            reply

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

let parsePiece (s:string) : Result<Piece, string> =
    let r = run (ppiece .>> eof) s
    match r with
    | Success (piece, _, _) -> Result.Success piece
    | Failure (msg, _, _) -> Result.Failure msg

let parseEmpties (s:string) : Result<Piece option list, string> =
    let r = run (pempties .>> eof) s
    match r with
    | Success (result, _, _) -> Result.Success result
    | Failure (msg, _, _) -> Result.Failure msg

let parseRank (s:string) : Result<Piece option list, string> = 
    let r = run (pRank .>> eof) s
    match r with
    | Success (result, _, _) -> Result.Success result
    | Failure (msg, _, _) -> Result.Failure msg
    