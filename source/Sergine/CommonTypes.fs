module CommonTypes

type Result<'TSuccess,'TFailure> = | Success of 'TSuccess | Failure of 'TFailure

type Player = | Black | White

type PieceKind = | Pawn | Knight | Bishop | Rook | Queen | King

type Piece = { Player : Player; Kind: PieceKind }

type CastlingSide = | Queenside | Kingside 

type AvailableCastling = { Player: Player; Side: CastlingSide }

type Coordinate = int * int

type Board = Map<Coordinate, Piece>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Board =

    let create (ranks:Piece option list list) : Board =
        List.mapi (fun r rank -> List.mapi (fun f s -> ((f,r),s)) rank) ranks
        |> List.concat
        |> List.map (fun ((f,r),s) -> match s with | Some p -> Some ((f,r),p) | _ -> None)
        |> List.choose (fun x -> x)
        |> Map.ofSeq        

type Position = {
    Board: Board;
    Turn: Player;
    Castlings: AvailableCastling list;
    EnPassantTarget: Coordinate option;
    HalfmoveCounter: int;
    FullmoveCounter: int
}

type Move = { Piece: Piece; Source: Coordinate; Target: Coordinate }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Move =
    let create piece source target = { Piece = piece; Source = source; Target = target }

module Algebraic =

    let ofCoordinate ((f, r) : Coordinate) =
        let fileName = char(int('a') + f)
        let rankName = char(int('1') + r)
        sprintf "%c%c" fileName rankName

    let toCoordinate (a : string) : Coordinate =
        let fileIndex = int(a.[0]) - int('a')
        let rankIndex = int(a.[1]) - int('1')
        (fileIndex, rankIndex)

