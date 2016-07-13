module Algebraic

open CommonTypes

let ofCoordinate ((f, r) : Coordinate) =
    let fileName = char(int('a') + f)
    let rankName = char(int('1') + r)
    sprintf "%c%c" fileName rankName

let toCoordinate (a : string) : Coordinate =
    let fileIndex = int(a.[0]) - int('a')
    let rankIndex = int(a.[1]) - int('1')
    (fileIndex, rankIndex)

let ofMove (move:Move) =
    if Option.isSome move.Promotion then
        sprintf "%s%sq" (ofCoordinate move.Source) (ofCoordinate move.Target)
    else
        sprintf "%s%s" (ofCoordinate move.Source) (ofCoordinate move.Target) 
