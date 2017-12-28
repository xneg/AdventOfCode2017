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
    | Up -> (x, y - 1)
    | Left -> (x - 1, y)
    | Down -> (x, y + 1)
    
let explode (s:string) =
        [for c in s -> c]
let problemFileName = @"Data\19.txt"

let field = problemFileName 
                |> processFile 
                |> Seq.map (explode >> List.toArray) 
                |> Seq.toArray  

let rec moveThroughTubes (field : char[][]) direction letters position =
    let getCurSymbol position = 
        let x, y = fst position, snd position
        // printfn "%d %d" x y
        if (x < 0 || x >= field.[0].Length || y < 0 || y >= field.Length) then ' '
        else field.[y].[x]

    let findNewDirection =
        match direction with
        | Up | Down -> if move position Left |> getCurSymbol <> ' ' then Left else Right
        | Left | Right -> if move position Up |> getCurSymbol <> ' ' then Up else Down

    let curSymbol = getCurSymbol position
    // printfn "%c" curSymbol
    match getCurSymbol position with
    | ' ' -> letters //exit
    | '|'| '-' -> move position direction |> moveThroughTubes field direction letters
    | '+' -> let direction = findNewDirection
             //printfn "%s" (direction.ToString())
             let newPos = move position Right
             let curSymbol = getCurSymbol newPos
             //printfn "%d %d" (fst newPos) (snd newPos)
             //printfn "newPos symbol = %c" curSymbol
             move position direction |> moveThroughTubes field direction letters
    | letter -> move position direction |> moveThroughTubes field direction (letter::letters)

let enter = 0, field.[0] |> Array.findIndex (fun x -> x = '|')      

let result = moveThroughTubes field Down [] (snd enter, fst enter) 
            |> List.rev
            |> Array.ofList |> System.String

