module Day05

// problem page
// http://adventofcode.com/2017/day/5

open System.IO

// conditions for two parts of puzzle
type MyPredicate<'a>  = 'a -> int
let fstPredicate : MyPredicate<int> = (fun _ -> 1)
let sndPredicate : MyPredicate<int> = fun x -> if x >= 3 then -1 else 1

let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        };;

// steps calculator
let steps predicate =

    let modifyArray position array modBy =
        Array.set array position (array.[position] + modBy)

    let rec processMaze currentPos steps (a : int[]) =
        if currentPos >= a.Length 
        then steps 
        else
            let nextPos = currentPos + a.[currentPos]
            modifyArray currentPos a (predicate a.[currentPos])
            processMaze nextPos (steps + 1) a
    processMaze 0 0        

let puzzlesolver predicate  =
    processFile >> Seq.map(int) >> Seq.toArray >> steps predicate

let result1 = puzzlesolver fstPredicate @"Data\05.txt" 
let result2 = puzzlesolver sndPredicate @"Data\05.txt"
