module Day13

// problem page
// http://adventofcode.com/2017/day/13

open System.IO
let problemFileName = @"Data\13.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split(':')
    };;

let input = problemFileName |> processFile |> Seq.map (fun x -> (int)x.[0], (int)x.[1]) |> Seq.toList

let rec modInput input acc =
    let generate = List.map (fun x -> (x, 0))
    match input with
    | [] -> acc
    | [a] -> a::acc
    | h::tail when fst h <> fst(Seq.head tail) - 1 ->
                let missing = generate [fst h + 1.. fst(Seq.head tail) - 1] |> List.rev
                modInput tail (missing @ h::acc)
    | h::tail -> modInput tail (h::acc)                    

let newInput = modInput input [] |> List.rev

let pingpong length step =
    let a = step % (2 * length - 2)
    if a < length - 1 then a else 2 * length - 2 - a

let rec calcSeverety sum step list =
    match list with
    | [] -> sum
    | h::tail -> 
        let hit = if pingpong (snd h) step = 0 then snd h * step else 0                  
        calcSeverety (sum + hit) (step + 1) tail 

let testList = [(0, 3); (1, 2); (2, 0); (3, 0); (4, 4); (5, 0); (6, 4)]

let testSeverety = testList |> calcSeverety 0 10

let tripSeverety = newInput |> calcSeverety 0 0

let rec findDelay delay input =
    if calcSeverety 0 delay input = 0 
    then delay 
    else findDelay (delay + 1) input

let delay = newInput |> findDelay 0

// let testList2 = [(0, 3); (1, 2); (4, 4); (6, 4)]

// let rec calcSeverety' sum step list =
//     match list with
//     | [] -> sum
//     | [a] -> sum + if pingpong (snd a) step = 0 then snd a * step else 0
//     | h::tail -> let hit = if pingpong (snd h) step = 0 then snd h * step else 0
//                  let stepDif = fst (List.head tail) - fst h
//                  calcSeverety' (sum + hit) (step + stepDif) tail 

// input |> calcSeverety' 0 0
