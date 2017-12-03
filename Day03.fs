module Day03

// problem page
// http://adventofcode.com/2017/day/3

type Direction =
    | Right 
    | Up 
    | Left 
    | Down 

let move (x, y) direction =
    match direction with
    | Right -> (x + 1, y)
    | Up -> (x, y + 1)
    | Left -> (x - 1, y)
    | Down -> (x, y - 1)

let nextDirection direction =
    let directions = [Right; Up; Left; Down]
    let nextPosition = (List.findIndex (fun x -> x = direction) directions) + 1
    List.item (nextPosition % 4) directions

let getDirection position direction =
    let nextPosition = move position direction
    let radius = max (abs (fst position))  (abs (snd position))
    if fst nextPosition = radius + 1 && snd nextPosition = - radius then direction
    elif abs (fst nextPosition) > radius || abs (snd nextPosition) > radius then nextDirection direction 
    else direction

let currentDirection = Right
let currentPosition = (0, 0)

let rec calc position direction l =
    match l with
    | [_] -> position 
    | _::tail -> 
        let newPosition = move position direction
        let newDirection = getDirection newPosition direction
        calc newPosition newDirection tail   
    | _ -> position  

let getNthElementPosition n = 
    [0..n-1] |> calc (0, 0) Right
    
let distance position = abs (fst position) + abs (snd position)

getNthElementPosition 12 |> distance
getNthElementPosition 1024 |> distance
getNthElementPosition 289326 |> distance

