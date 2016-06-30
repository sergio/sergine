module Parser

open CommonTypes

let parseFen (fen : string) : Result<Position, string> =
    if(fen.Contains(" w ")) then
        Success { PlayerInTurn = White }
    else if(fen.Contains(" b ")) then
        Success { PlayerInTurn = Black }
    else
        Failure "Player in turn is neither 'w' nor 'b'."

