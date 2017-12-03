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

let puzzle_input = 289326

getNthElementPosition 12 |> distance
getNthElementPosition 1024 |> distance
getNthElementPosition puzzle_input |> distance

let globalMatrix : int [,] = Array2D.init 20 20 (fun _ _ -> 0)

let geometryToArray position =
    let arrayPos = (Array2D.length1 globalMatrix / 2 + fst position, Array2D.length2 globalMatrix / 2 + snd position)
    globalMatrix.[fst arrayPos, snd arrayPos]

let neighbours position =
    [for i in (fst position) - 1 .. (fst position) + 1 do
        for j in (snd position) - 1.. (snd position) + 1 do
            yield (i, j)]

let getCellValue =
    neighbours >> List.sumBy (geometryToArray)

let setValueToArray position =
    let arrayPos = (Array2D.length1 globalMatrix / 2 + fst position, Array2D.length2 globalMatrix / 2 + snd position)
    Array2D.set globalMatrix (fst arrayPos) (snd arrayPos)

let rec fillMatrix position direction l =
    let cellValue = getCellValue position
    setValueToArray position cellValue
    match l with
    | _ when cellValue > puzzle_input -> cellValue
    | [_] -> 
        0
    | _::tail -> 
        let newPosition = move position direction
        let newDirection = getDirection newPosition direction
        fillMatrix newPosition newDirection tail  
    | _ -> 
        0

let fillNthElementPosition n = 
    setValueToArray (0, 0) 1
    [0..n-1] |> fillMatrix (0, 0) Right

let result = fillNthElementPosition 70

result;;
