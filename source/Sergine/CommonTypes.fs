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
    
    let piecesOf (board:Board) (player:Player) : (Coordinate * Piece) seq =
        board 
        |> Seq.filter (fun (KeyValue(coord,piece)) -> piece.Player = player)
        |> Seq.map (fun (KeyValue(coord,piece)) -> (coord,piece))

    let pieceAt (board:Board) (square:Coordinate) : Piece option =
        if board.ContainsKey square then
            Some board.[square]
        else
            None

type Position = {
    Board: Board;
    Turn: Player;
    Castlings: AvailableCastling list;
    EnPassantTarget: Coordinate option;
    HalfmoveCounter: int;
    FullmoveCounter: int
}

type Move = { Piece: Piece; Source: Coordinate; Target: Coordinate; Promotion: PieceKind option }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Move =
    let create piece source target = { Piece = piece; Source = source; Target = target; Promotion = None  }
    let createWithPromotion piece source target promotion = { Piece = piece; Source = source; Target = target; Promotion = Some promotion }
