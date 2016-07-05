module CommonTypes

type Result<'TSuccess,'TFailure> = | Success of 'TSuccess | Failure of 'TFailure

type Player = | Black | White
type PieceKind = | Pawn | Knight | Bishop | Rook | Queen | King
type Piece = { Player : Player; Kind: PieceKind }
type Square = Piece option
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
