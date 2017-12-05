module Day05

open System.IO

let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        };;

let array = [|0;3;0;1;-3|]

let modifyArray position a modBy =
    Array.set a position (a.[position] + modBy)

let rec processMaze currentPos steps (a : int[]) =
    if currentPos >= a.Length 
    then steps 
    else
        let nextPos = currentPos + a.[currentPos]
        modifyArray currentPos a 1
        processMaze nextPos (steps + 1) a
let steps = processMaze 0 0

let result = processFile "data.txt" |> Seq.map(int) |> Seq.toArray |> steps

steps array
let array2 = [|0;3;0;1;-3|]

let rec processMaze2 currentPos steps (a : int[]) =
    if currentPos >= a.Length 
    then steps 
    else
        let nextPos = currentPos + a.[currentPos]
        let modBy = if a.[currentPos] >= 3 then - 1 else 1
        modifyArray currentPos a modBy
        processMaze2 nextPos (steps + 1) a

let steps2 = processMaze2 0 0    

steps2 array2

let result2 = processFile "data.txt" |> Seq.map(int) |> Seq.toArray |> steps2

type MyPredicate<'a>  = 'a -> int

type SimplePredicate = int -> int

let myFunc: MyPredicate<int> = fun x -> if x >= 3 then -1 else 1

let myFunc2: MyPredicate<float> = fun x -> if x >= 3. then -1 else 1
