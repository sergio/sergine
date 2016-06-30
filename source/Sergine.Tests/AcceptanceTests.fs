module AcceptanceTests

open Sergine
open NUnit.Framework
open Swensen.Unquote

[<Test>]
[<Category("Acceptance")>]
let ``Engine suggests a valid move for white as the best move for the start position`` () =

    let START_POSITION_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 
    let validMovesForWhite = ["a2a3"; "b2b3"; "c2c3"; "d2d3"; "e2e3"; "f2f3"; "g2g3"; "h2h3"; 
        "a2a4"; "b2b4"; "c2c4"; "d2d4"; "e2e4"; "f2f4"; "g2g4"; "h2h4";
        "Nb1a3"; "Nb1c3"; "Ng1f3"; "Ng1h3"]

    let suggestedMove = Engine.bestMoveForPosition START_POSITION_FEN

    test <@ Seq.exists ((=) suggestedMove) validMovesForWhite @>
