module ParserTests

open NUnit.Framework
open Swensen.Unquote
open CommonTypes
open Parser

let isParsingFailure(result) = 
    match result with
    | Failure s -> true
    | _ -> false

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
        test <@ isParsingFailure result @>

module ``When parsing castling options`` =

    [<Test>]
    let ``returns empty list for '-'`` () =
        let result = parseCastlings "-"
        test <@ result = Success [] @>

    [<Test>]
    let ``returns queenside white for 'Q'`` () =
        let result = parseCastlings "Q"
        test <@ result = Success [{Side = Queenside; Player = White }] @>

    [<Test>]
    let ``returns all options for 'QKqk'`` () =
        let result = parseCastlings "QKqk"
        let expectedCastlings = Set.ofList [
                                            {Player = White; Side = Queenside};
                                            {Player = Black; Side = Queenside};
                                            {Player = White; Side = Kingside};
                                            {Player = Black; Side = Kingside}
                                           ]
        test <@ match result with
                | Success castlings -> castlings |> Set.ofList = expectedCastlings
                | Failure _ -> false @>

    [<Test>]
    let ``returns error if unknown input`` () =
        let result = parseCastlings "QkA"
        test <@ isParsingFailure result @>

module ``When parsing en passant capture target square`` =

    [<Test>]
    let ``returns None for '-'`` () =
        let result = parseEnPassantTarget "-"
        test <@ result = Success None @>

    [<Test>]
    let ``returns correct coordinate for 'a1'`` () =
        let result = parseEnPassantTarget "a1"
        test <@ result = Success (Some (0, 0)) @>

    [<Test>]
    let ``returns correct coordinate for 'h8'`` () =
        let result = parseEnPassantTarget "h8"
        test <@ result = Success (Some (7, 7)) @>
