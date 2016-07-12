module Movement

open CommonTypes

type MovePredicate = Move -> bool
let (<&&>) (f:MovePredicate) (g:MovePredicate) : MovePredicate = fun x -> f x && g x
let (<||>) (f:MovePredicate) (g:MovePredicate) : MovePredicate = fun x -> f x || g x

module Vector2D =
    let scale (k:int) (x,y) = k * x, k * y 
    let sum (x1,y1) (x2,y2) = x1 + x2, y1 + y2
    let invert (x,y) = (-x,-y)

module Dirs =
    let NORTH = (0,1)
    let SOUTH = (0,-1)
    let WEST = (-1,0)
    let EAST = (1,0)
    let NE = Vector2D.sum NORTH EAST
    let NW = Vector2D.sum NORTH WEST
    let SE = Vector2D.sum SOUTH EAST
    let SW = Vector2D.sum SOUTH WEST
    let HORIZVERT = [NORTH; SOUTH; WEST; EAST]
    let DIAGONAL = [NE; SE; SW; NW]
    let ALL = HORIZVERT @ DIAGONAL 
    let KNIGHT = [(1,2);(1,-2);(-1,2);(-1,-2);(2,1);(2,-1);(-2,1);(-2,-1)]
    let relativeTo player directions =
        match player with
        | White -> directions
        | Black -> directions |> List.map Vector2D.invert

let private inBoard (x,y) = 0 <= x && x <= 7 && 0 <= y && y <= 7

let private isPawnInStartPosition square piece =
    let rank (f,r) = r 
    match piece with
    | { Kind = Pawn; Player = White } -> rank square = 1
    | { Kind = Pawn; Player = Black } -> rank square = 6
    | _ -> false

let private availableMovesInDirection isEmptySquare square piece counter movepredicate direction : Move list =
    let generator = (fun distance -> 
        Vector2D.sum square (Vector2D.scale (distance + 1) direction))
    generator
    |> counter
    |> Seq.takeWhile inBoard
    |> Seq.takeWhileInclusive isEmptySquare
    |> Seq.map (fun target -> { Piece = piece; Source = square; Target = target })
    |> Seq.filter movepredicate
    |> List.ofSeq

let availableMovesFromSquare (pieceAt:Coordinate -> Piece option) (enPassantTarget:Coordinate option) square : Set<Move> =

    let unlimited = Seq.initInfinite

    let upto howmany = Seq.init howmany

    let isEmptySquare square = pieceAt square |> Option.isNone

    let targetSquareIsEnemyPiece : MovePredicate =
        fun move -> 
            match pieceAt move.Target with 
            | Some otherPiece -> otherPiece.Player <> move.Piece.Player
            | None -> false 

    let targetIsEnPassantTarget : MovePredicate =
        fun move ->
            match enPassantTarget with
            | None -> false 
            | Some square -> move.Target = square 

    let targetSquareIsEmpty : MovePredicate =
        fun move -> isEmptySquare move.Target  

    let isEmptyOrCapture : MovePredicate = targetSquareIsEmpty <||> targetSquareIsEnemyPiece

    match pieceAt square with
    | Some piece ->
        let recipe = 
            match piece.Kind with 
            | Rook -> [(Dirs.HORIZVERT, unlimited, isEmptyOrCapture)]
            | Bishop -> [(Dirs.DIAGONAL, unlimited, isEmptyOrCapture)]
            | Queen -> [(Dirs.ALL, unlimited, isEmptyOrCapture)]
            | King -> [(Dirs.ALL, upto 1, isEmptyOrCapture)]
            | Knight -> [(Dirs.KNIGHT, upto 1, isEmptyOrCapture)]
            | Pawn -> 
                let allowedDistance = if (isPawnInStartPosition square piece) then 2 else 1
                [([Dirs.NORTH], upto allowedDistance, targetSquareIsEmpty)]
            @ [([Dirs.NE; Dirs.NW], upto 1, targetSquareIsEnemyPiece <||> targetIsEnPassantTarget)]
        recipe 
        |> List.map (fun (directions, counter, movePredicate) ->
                    directions 
                    |> Dirs.relativeTo piece.Player 
                    |> List.map (availableMovesInDirection isEmptySquare square piece counter movePredicate)
                    |> List.concat)
        |> List.concat
        |> Set.ofList
    | None -> Set.empty
