module Duet

// problem page
// http://adventofcode.com/2017/day/18

open System.Collections.Generic
open System.IO

let problemFileName = @"Data\18.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split([|' '|])
    };;

let registers = new Dictionary<string, int>()

let mutable lastSoundPlayed = 0;
let mutable currIns = 0;

let getValue x = if registers.ContainsKey x then registers.[x] else 0

let snd x = lastSoundPlayed <- getValue x

let set x value = if registers.ContainsKey x then registers.[x] <- value else registers.Add (x, value)

let add x value = set x (getValue x + value)

let mul x value = set x (getValue x * value)

let _mod x value = set x (getValue x % value)

let rcv x = if getValue x <> 0 then lastSoundPlayed else 0

let jgz x value = currIns <- currIns + if getValue x > 0 then value - 1 else 0

let instructions = problemFileName
                |> processFile
                |> Seq.toArray

let doInstruction instruction = 
    let getSndValue str =
        match System.Int32.TryParse str with
        | true, num -> num
        | _ -> getValue str

    match instruction with 
    | [|a;x|] when a = "snd" -> snd x 
    | [|a;x|] when a = "rcv" -> let recover = rcv x 
                                recover |> printfn "rcv %d" 
                                if recover <> 0 then currIns <- 10000       
    | [|a;x;c|] ->  let value = getSndValue c
                    if a = "set" then set x value
                    elif a = "add" then add x value
                    elif a = "mul" then mul x value
                    elif a = "mod" then _mod x value
                    elif a = "jgz" then jgz x value

currIns <- 0
registers.Clear |> ignore

let rec playSounds () =
    if currIns < instructions.Length then
        doInstruction instructions.[currIns]
        currIns <- currIns + 1 
        playSounds ()
    else
        () 

playSounds ();;  
