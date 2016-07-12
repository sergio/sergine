module MovementTests

open CommonTypes
open Movement
open NUnit.Framework
open Swensen.Unquote

let coord = Algebraic.toCoordinate
let alg = Algebraic.ofCoordinate
let at = ()
let a player kind at square = (square, { Player = player; Kind = kind})

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
            let source, piece = a White Rook at "e3"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["e1"; "e2"; "e4"; "e5"; "e6"; "e7"; "e8"; "a3"; "b3"; "c3"; "d3"; "f3"; "g3"; "h3"] 
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =
            let source, piece = a White Rook at "h8"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["h7";"h6";"h5";"h4";"h3";"h2";"h1";"g8";"f8";"e8";"d8";"c8";"b8";"a8"]
            test <@ actualMoves = expectedMoves @>

module ``Bishop movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e3`` () =
            let source, piece = a White Bishop at "e3"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["c1";"d2";"f4";"g5";"h6";"a7";"b6";"c5";"d4";"f2";"g1"]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =
            let source, piece = a White Bishop at "h8"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["a1";"b2";"c3";"d4";"e5";"f6";"g7"]
            test <@ actualMoves = expectedMoves @>

module ``Queen movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e3`` () =
            let source, piece = a White Queen at "e3"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece (["c1";"d2";"f4";"g5";"h6";"a7";"b6";"c5";"d4";"f2";"g1"] @ ["e1"; "e2"; "e4"; "e5"; "e6"; "e7"; "e8"; "a3"; "b3"; "c3"; "d3"; "f3"; "g3"; "h3"])
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =
            let source, piece = a White Queen at "h8"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece (["a1";"b2";"c3";"d4";"e5";"f6";"g7"] @ ["h7";"h6";"h5";"h4";"h3";"h2";"h1";"g8";"f8";"e8";"d8";"c8";"b8";"a8"])
            test <@ actualMoves = expectedMoves @>

module ``Knight movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e4`` () =
            let source, piece = a White Knight at "e4"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["d2";"d6";"f2";"f6";"c3";"c5";"g3";"g5"]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from g7`` () =
            let source, piece = a White Knight at "g7"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["f5";"h5";"e6";"e8";]
            test <@ actualMoves = expectedMoves @>

module ``King movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e3`` () =
            let source, piece = a White King at "e3"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["d2";"e2";"f2";"d3";"f3";"d4";"e4";"f4";]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =
            let source, piece = a White King at "h8"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["h7";"g8";"g7";]
            test <@ actualMoves = expectedMoves @>

module ``Pawn movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``White pawn can move 2 squares from start position`` () =
            let source, piece = a White Pawn at "b2"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["b3";"b4"]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``White pawn can move only 1 square if not in start position`` () =
            let source, piece = a White Pawn at "f4"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["f5";]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Black pawn can move 2 squares from start position`` () =
            let source, piece = a Black Pawn at "b7"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["b6";"b5"]
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Black pawn can move only 1 square if not in start position`` () =
            let source, piece = a Black Pawn at "f4"
            let actualMoves = availableMovesForPiece (boardWithPieces [source, piece]) (coord source) piece
            let expectedMoves = movesToTargetSquares source piece ["f3";]
            test <@ actualMoves = expectedMoves @>

    module ``When pawn is blocked`` = 
        [<Test>]
        let ``Pawn cannot move if blocked by same color piece`` () =
            let source, piece = a White Pawn at "c2"
            let board = boardWithPieces [source, piece; a White Rook at "c3"]
            let actualMoves = availableMovesForPiece board (coord source) piece
            let expectedMoves : Set<Move> = Set.empty
            test <@ actualMoves = expectedMoves @>

        let ``Pawn cannot move if blocked by enemy piece`` () =
            let source, piece = a White Pawn at "c2"
            let board = boardWithPieces [source, piece; a Black Rook at "c3"]
            let actualMoves = availableMovesForPiece board (coord source) piece
            let expectedMoves : Set<Move> = Set.empty
            test <@ actualMoves = expectedMoves @>
