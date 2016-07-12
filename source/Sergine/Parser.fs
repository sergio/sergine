module Parser

open System
open CommonTypes
open FParsec
open FParsec.CharParsers

let blank : Parser<unit, unit> = skipChar ' '

let pHalfmove = pint32

let pFullMove = pint32

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

let pboard : Parser<Board, unit> = sepBy1 pRank (pchar '/') |>> (fun ranks -> ranks |> List.rev |> Board.create)

let pipe6 p1 p2 p3 p4 p5 p6 f =
    pipe4 p1 p2 p3 (tuple3 p4 p5 p6)
          (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)    

let pposition : Parser<CommonTypes.Position, unit> = 
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

let parseFen (fen : string) : Result<CommonTypes.Position, string> =
    let player = (charReturn 'w' White) <|> (charReturn 'b' Black) 
    let playerBetweenSpaces = spaces1 >>. player .>> spaces1
    let result = run pposition fen
    match result with
    | Success (result, _, _) -> Result.Success result 
    | Failure (errorMessage, _, _) -> Result.Failure errorMessage 
