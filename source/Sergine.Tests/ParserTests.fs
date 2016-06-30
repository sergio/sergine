module ParserTests

open NUnit.Framework
open Swensen.Unquote
open CommonTypes
open Parser

[<Test>]
let ``Parser returns player in turn as White when 'w'`` () =
    let position = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let expectedPlayer = Success White
    test <@ position = Success { PlayerInTurn= White } @>

[<Test>]
let ``Parser returns player in turn as Black when 'b'`` () =
    let position = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"
    test <@ position = Success { PlayerInTurn= Black }@>

[<Test>]
let ``Parser returns error when player is not 'w' or 'b'`` () =
    let result = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1"
    test <@ 
            match result with
            | Failure s -> true
            | _ -> false
     @>
