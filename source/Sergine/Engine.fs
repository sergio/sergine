module Engine

open CommonTypes
open Parser
open Movement

let availableMovesForPlayerInTurn (position : Position) =
    let playerPieces = Board.piecesOf position.Board position.Turn
    let pieceAt = Board.pieceAt position.Board
    playerPieces
    |> Seq.map (fun (square, piece) -> square)
    |> Seq.map (availableMovesFromSquare pieceAt position.EnPassantTarget)    
    |> Set.unionMany
    |> Set.toList

let selectBestMove (position : Position) : Result<string, string> =
    let moves = availableMovesForPlayerInTurn position
    match moves with
    | [] -> Failure "No moves available for player in turn"
    | x :: xs -> Success (Algebraic.ofMove x)

let bestMoveForPosition (fen:string) : Result<string, string> =
    let parserResult = parseFen fen
    match parserResult with
    | Success position -> selectBestMove position
    | Failure msg -> Failure msg

