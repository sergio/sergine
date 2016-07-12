module MovementTests

open CommonTypes
open Movement
open NUnit.Framework
open Swensen.Unquote

let a p k = Some { Player = p; Kind = k }
let coord = Algebraic.toCoordinate
let alg = Algebraic.ofCoordinate

let movesToTargetSquares source piece targetSquares =
    targetSquares
    |> List.map (fun target -> Move.create piece (coord source) (coord target))
    |> Set.ofList

let boardWithPieces boardContents =
    let piecesByCoordinate = 
        boardContents
        |> List.map (fun (square, piece) -> (coord square, piece))
        |> Map.ofList
    fun (square:Coordinate) ->
        if piecesByCoordinate.ContainsKey square then
            Some piecesByCoordinate.[square]
        else
            None

module ``Rook movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e3`` () =

            let source, piece = "e3", { Player = White; Kind = Rook }
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["e1"; "e2"; "e4"; "e5"; "e6"; "e7"; "e8"; "a3"; "b3"; "c3"; "d3"; "f3"; "g3"; "h3"] 
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =

            let source, piece = "h8", { Player = White; Kind = Rook }
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["h7";"h6";"h5";"h4";"h3";"h2";"h1";"g8";"f8";"e8";"d8";"c8";"b8";"a8"]
            test <@ actualMoves = expectedMoves @>

module ``Bishop movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e3`` () =

            let source, piece = "e3", { Player = White; Kind = Bishop }
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["c1";"d2";"f4";"g5";"h6";"a7";"b6";"c5";"d4";"f2";"g1"]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =

            let source, piece = "h8", { Player = White; Kind = Bishop }
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["a1";"b2";"c3";"d4";"e5";"f6";"g7"]
            test <@ actualMoves = expectedMoves @>
