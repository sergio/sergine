module CommonTypes

type Result<'TSuccess,'TFailure> = | Success of 'TSuccess | Failure of 'TFailure

type Player = | Black | White

type Position = { PlayerInTurn : Player}