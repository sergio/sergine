module ParserTests

open NUnit.Framework
open Swensen.Unquote
open CommonTypes
open Parser

module ``When parsing the player in turn`` =
    [<Test>]
    let ``returns White when 'w'`` () =
        let playerInTurn = parseTurn "w"
        test <@ playerInTurn = Success White @>

    [<Test>]
    let ``returns Black when 'b'`` () =
        let playerInTurn = parseTurn "b"
        test <@ playerInTurn = Success Black @>

    [<Test>]
    let ``returns failure when not 'w' nor 'b'`` () =
        let result = parseTurn "x"
        test <@ match result with
                | Failure s -> true
                | _ -> false
            @>

