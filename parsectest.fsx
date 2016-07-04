#r "source/packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "source/packages/FParsec/lib/net40-client/FParsec.dll"
#r @"System.dll"

open FParsec
open FParsec.CharParsers
open System

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let START_POSITION = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

test pfloat "1.2"

type Player = | Black | White
type PieceKind = | Pawn | Knight | Bishop | Rook | Queen | King
type Piece = { Player : Player; Kind: PieceKind }
type Square = Piece option
type CastlingSide = | Queenside | Kingside 
type AvailableCastling = { Player: Player; Side: CastlingSide }
type Coordinate = String
type Position = {
    Board: Piece option list list;
    Turn: Player;
    Castlings: AvailableCastling list;
    EnPassantTarget: Coordinate option;
    HalfmoveCounter: int;
    FullmoveCounter: int
}

let ppiece : Parser<Piece option, unit> =
    let a p k = Some { Player = p; Kind = k } 
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

let ppieces : Parser<Piece option list, unit> = many1 ppiece

let empties : Parser<Piece option list, unit> =
    anyOf "12345678" |>> (fun c -> c |> string |> Int32.Parse |> List.replicate <| None )

let asciiSquare (square : Piece option) : char =
    match square with
    | None -> '_'
    | Some { Player = player; Kind = kind } 
        -> match player, kind with
        | Black, Pawn -> '\u265F'
        | Black, Knight -> '\u265E'
        | Black, Bishop -> '\u265D'
        | Black, Rook -> '\u265C'
        | Black, Queen -> '\u265B'
        | Black, King -> '\u265A'
        | White, Pawn -> '\u2659'
        | White, Knight -> '\u2658'
        | White, Bishop -> '\u2657'
        | White, Rook -> '\u2656'
        | White, Queen -> '\u2655'
        | White, King -> '\u2654'

// Parser

let blank : Parser<unit, unit> = skipChar ' '

let prank = many1 (ppieces <|> empties) |>> List.concat

let pboard : Parser<Piece option list list, unit> = sepBy1 prank (pchar '/')

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

let pEnPassantSquare : Parser<Coordinate option, unit> =
    ( charReturn '-' None
    <|> (
            anyOf "abcdefgh" .>>. anyOf "12345678" |>> (fun (f, r) -> Some (string [| f; r |])) 
        )
    )

let pHalfmove = pint32

let pFullMove = pint32

let pipe6 p1 p2 p3 p4 p5 p6 f =
    pipe4 p1 p2 p3 (tuple3 p4 p5 p6)
          (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)    

let pposition : Parser<Position, unit> = 
    let makePosition b t c p h f = {
        Board = b;
        Turn = t;
        Castlings = c;
        EnPassantTarget = p;
        HalfmoveCounter = h;
        FullmoveCounter = f
    }
    pipe6 
        (pboard .>> blank) 
        (pturn .>> blank) 
        (pCastlings .>> blank) 
        (pEnPassantSquare .>> blank) 
        (pHalfmove .>> blank) 
        pFullMove 
        makePosition

let fenParser = pposition .>> eof

let position = run fenParser "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 3"

// let printBoard board : unit =
//     List.iter (fun rank ->
//         List.iter (fun c -> printf "%c" (asciiSquare c)) rank
//     ) board

// match position with
//     | Success (board, _, _) -> printBoard board
//     | Failure (error, _, _) -> printfn "%A" error
