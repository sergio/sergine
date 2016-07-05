module Engine

open CommonTypes
open Parser

let availableMovesForPlayerInTurn (position : Position) = []

let selectBestMove (position : Position) : Result<string, string> =
    let moves = availableMovesForPlayerInTurn position
    match moves with
    | [] -> Failure "No moves available for player in turn"
    | x :: xs -> Success x

let bestMoveForPosition (fen:string) : Result<string, string> =
    let parserResult = parseFen fen
    match parserResult with
    | Success position -> selectBestMove position
    | Failure msg -> Failure msg

