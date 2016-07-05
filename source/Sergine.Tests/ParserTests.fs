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
        let expectedCastlings =
            Set.ofList [ {Player = White; Side = Queenside};
                         {Player = Black; Side = Queenside};
                         {Player = White; Side = Kingside};
                         {Player = Black; Side = Kingside} ]
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

module ``When parsing board contents`` =

    [<Test>]
    let ``returns Black Rook for 'r'`` () =
        let result = parsePiece "r"
        test <@ result = Success { Player = Black; Kind = Rook } @>

    [<Test>]
    let ``returns White Knight for 'N'`` () =
        let result = parsePiece "N"
        test <@ result = Success { Player = White; Kind = Knight } @>

    [<Test>]
    let ``returns error for unknown pieces`` () =
        let result = parsePiece "J"
        test <@ isParsingFailure result @>

    [<Test>]
    let ``returns sequence of 8 empty squares for '8'`` () =
        let result = parseEmpties "8"
        test <@ result = Success (None |> List.replicate 8) @>

    [<Test>]
    let ``returns sequence of 3 empty squares for '3'`` () =
        let result = parseEmpties "3"
        test <@ result = Success (None |> List.replicate 3) @>

    [<Test>]
    let ``returns failure for more than 8 empty squares`` () =
        let result = parseEmpties "9"
        test <@ isParsingFailure result @>

    [<Test>]
    let ``returns failure for more than 8 pieces in a rank`` () =
        let result = parseRank "ppppppppp"
        test <@ isParsingFailure result @>
    
    [<Test>]
    let ``returns failure for less than 8 pieces in a rank`` () =
        let result = parseRank "pp2"
        test <@ isParsingFailure result @>

    [<Test>]
    let ``returns correct combination of pieces for 'PpP2Kq1'`` () =
        let result = parseRank "PpP2Kq1"
        let expected = [
            Some { Player = White; Kind = Pawn };
            Some { Player = Black; Kind = Pawn };
            Some { Player = White; Kind = Pawn };
            None;
            None;
            Some { Player = White; Kind = King };
            Some { Player = Black; Kind = Queen };
            None;
        ]
        test <@ result = Success expected @>
        
