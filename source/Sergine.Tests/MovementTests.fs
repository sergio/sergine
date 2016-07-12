module MovementTests

open CommonTypes
open Movement
open NUnit.Framework
open Swensen.Unquote

let a p k = Some { Player = p; Kind = k }
let coord = Algebraic.toCoordinate
let alg = Algebraic.ofCoordinate

module ``Rook movement`` =

    module ``When board is empty`` =
        [<Test>]
        let ``Returns correct moves from e3`` () =

            let piece = { Player = White; Kind = Rook }

            let board_stub square =
                match alg square with
                | "e3" -> Some piece
                | _ -> None

            let actualMoves = availableMovesForPiece board_stub (coord "e3") piece

            let expectedMoves = 
                ["e1"; "e2"; "e4"; "e5"; "e6"; "e7"; "e8"; "a3"; "b3"; "c3"; "d3"; "f3"; "g3"; "h3"]
                |> List.map (fun target -> Move.create piece (coord "e3") (coord target))
                |> Set.ofList
            
            test <@ actualMoves = expectedMoves @>

        [<Test>]
        let ``Returns correct moves from h8`` () =

            let piece = { Player = White; Kind = Rook }

            let board_stub square =
                match alg square with
                | "h8" -> Some piece
                | _ -> None

            let actualMoves = availableMovesForPiece board_stub (coord "h8") piece

            let expectedMoves = 
                ["h7";"h6";"h5";"h4";"h3";"h2";"h1";"g8";"f8";"e8";"d8";"c8";"b8";"a8"]
                |> List.map (fun target -> Move.create piece (coord "h8") (coord target))
                |> Set.ofList
            
            test <@ actualMoves = expectedMoves @>
