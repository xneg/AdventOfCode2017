module Day25

// problem page
// http://adventofcode.com/2017/day/19


open System.IO

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

let move (x, y) direction =
    match direction with
    | Right -> (x + 1, y)
    | Up -> (x, y + 1)
    | Left -> (x - 1, y)
    | Down -> (x, y - 1)
    
let explode (s:string) =
        [for c in s -> c]
let problemFileName = @"Data\19.txt"

let x = problemFileName 
                |> processFile 
                |> Seq.map (explode >> List.toArray) 
                |> Seq.toArray  

let enter = 0, x.[0] |> Array.findIndex (fun x -> x = '|')      