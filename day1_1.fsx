open System

type TurnDirection = 
    | Left
    | Right

type Heading =
    | North
    | East
    | South
    | West

type Position = {
    Heading: Heading
    X: int
    Y: int
}

type Move = {
    Direction: TurnDirection
    Distance: int
}

let input = "R3, L5, R2, L1, L2, R5, L2, R2, L2, L2, L1, R2, L2, R4, R4, R1, L2, L3, R3, L1, R2, L2, L4, R4, R5, L3, R3, L3, L3, R4, R5, L3, R3, L5, L1, L2, R2, L1, R3, R1, L1, R187, L1, R2, R47, L5, L1, L2, R4, R3, L3, R3, R4, R1, R3, L1, L4, L1, R2, L1, R4, R5, L1, R77, L5, L4, R3, L2, R4, R5, R5, L2, L2, R2, R5, L2, R194, R5, L2, R4, L5, L4, L2, R5, L3, L2, L5, R5, R2, L3, R3, R1, L4, R2, L1, R5, L1, R5, L1, L1, R3, L1, R5, R2, R5, R5, L4, L5, L5, L5, R3, L2, L5, L4, R3, R1, R1, R4, L2, L4, R5, R5, R4, L2, L2, R5, R5, L5, L2, R4, R4, L4, R1, L3, R1, L1, L1, L1, L4, R5, R4, L4, L4, R5, R3, L2, L2, R3, R1, R4, L3, R1, L4, R3, L3, L2, R2, R2, R2, L1, L4, R3, R2, R2, L3, R2, L3, L2, R4, L2, R3, L4, R5, R4, R1, R5, R3"

let createMove(x: string) = 
    let direction = 
        match x.[0] with
        | 'R' -> TurnDirection.Right
        | 'L' -> TurnDirection.Left
    let distance = x.Substring(1) |> Convert.ToInt32
    {Direction = direction; Distance = distance}
    
let convert (x:string) = 
    x.Split ([|','; ' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map createMove

let startPosition = {Heading = Heading.North; X = 0; Y = 0}

let applyMove (position:Position) (move:Move) =
    let heading = 
        match (position.Heading, move.Direction) with
        | (Heading.North, TurnDirection.Left) -> Heading.West
        | (Heading.North, TurnDirection.Right) -> Heading.East
        | (Heading.East, TurnDirection.Left) -> Heading.North
        | (Heading.East, TurnDirection.Right) -> Heading.South
        | (Heading.South, TurnDirection.Left) -> Heading.East
        | (Heading.South, TurnDirection.Right) -> Heading.West
        | (Heading.West, TurnDirection.Left) -> Heading.South
        | (Heading.West, TurnDirection.Right) -> Heading.North
    let x, y = 
        match heading with
        | Heading.North -> (position.X, position.Y + move.Distance)
        | Heading.South -> (position.X, position.Y - move.Distance)
        | Heading.East -> (position.X + move.Distance, position.Y)
        | Heading.West -> (position.X - move.Distance, position.Y)
    {Heading = heading; X = x; Y = y}
    
let calculateDistanceFromStart (position:Position) =
    Math.Abs position.X + Math.Abs position.Y

let moves = convert input 

let result = convert input 
                |> Seq.fold applyMove startPosition
                |> calculateDistanceFromStart

Console.WriteLine result