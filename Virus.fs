// module Virus

// problem page
// http://adventofcode.com/2017/day/22

open System.IO
open System.Web

let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

type Direction =
    | Right 
    | Up 
    | Left 
    | Down 

type Status = 
    | Clean
    | Weakend
    | Infected
    | Flagged

let nextStatus status = 
    let statuses = [Clean; Weakend; Infected; Flagged]
    let nextStatusPos = (List.findIndex (fun x -> x = status) statuses) + 1
    List.item (nextStatusPos % 4) statuses

let move (x, y) direction =
    match direction with
    | Right -> (x + 1, y)
    | Up -> (x, y + 1)
    | Left -> (x - 1, y)
    | Down -> (x, y - 1)

let turn currentDirection direction =
    let directions = [Right; Up; Left; Down]
    let diff = match direction with
                | Right -> -1
                | Left -> 1
                | Up -> 0
                | Down -> 2
    let nextPosition = (List.findIndex (fun x -> x = currentDirection) directions) + diff
    let nextPosition = if nextPosition < 0 then 4 + nextPosition else nextPosition
    List.item (nextPosition % 4) directions

let globalMatrix : int [,] = Array2D.init 501 501 (fun _ _ -> 0)

let geometryToArray position =
    let offset = Array2D.length1 globalMatrix / 2
    globalMatrix.[-snd position + offset, fst position + offset]

let setValueToArray position =
    let offset = Array2D.length1 globalMatrix / 2
    Array2D.set globalMatrix (-snd position + offset) (fst position + offset)

let problemFileName = @"Data\22.txt"     

let explode (s:string) =
        [for c in s -> c]

let initial =   problemFileName 
                |> processFile 
                |> Seq.map (explode >> List.toArray) 
                |> Seq.toArray

let offset = initial.Length / 2

for i in 0..initial.Length - 1 do
    for j in 0..initial.Length - 1 do
        let value = initial.[i].[j] = '.'
        let value = if initial.[i].[j] = '.' then 0 else 1
        setValueToArray (j - offset, -i + offset) value

// globalMatrix  
 
let rec virusGo position direction n limit infections =
    if n < limit then
        match geometryToArray position with
        | 0 -> 
                    let newDirection = turn direction Left
                    setValueToArray position 1
                    let newPosition = move position newDirection      
                    virusGo newPosition newDirection (n + 1) limit (infections + 1)
        | 1 -> 
                    let newDirection = turn direction Right
                    setValueToArray position 0
                    let newPosition = move position newDirection    
                    virusGo newPosition newDirection (n + 1) limit infections
    else
        setValueToArray position (geometryToArray position + 2)  
        infections

let iniPosition = (0, 0)      

let infections = virusGo iniPosition Up 0 10000 0    

// globalMatrix  