module CommonTypes

type Result<'TSuccess,'TFailure> = | Success of 'TSuccess | Failure of 'TFailure

type Player = | Black | White

type PieceKind = | Pawn | Knight | Bishop | Rook | Queen | King

type Piece = { Player : Player; Kind: PieceKind }

type CastlingSide = | Queenside | Kingside 

type AvailableCastling = { Player: Player; Side: CastlingSide }

type Coordinate = int * int

type Position = {
    Board: Piece option list list;
    Turn: Player;
    Castlings: AvailableCastling list;
    EnPassantTarget: Coordinate option;
    HalfmoveCounter: int;
    FullmoveCounter: int
}

module Algebraic =

    let ofCoordinate ((f, r) : Coordinate) =
        let fileName = char(int('a') + f)
        let rankName = char(int('1') + r)
        sprintf "%c%c" fileName rankName

    let toCoordinate (a : string) : Coordinate =
        let fileIndex = int(a.[0]) - int('a')
        let rankIndex = int(a.[1]) - int('1')
        (fileIndex, rankIndex)

