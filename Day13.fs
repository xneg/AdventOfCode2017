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

let getSeverety initialDelay input =
    let pingpong length step =
        let a = step % (2 * length - 2)
        if a < length - 1 then a else 2 * length - 2 - a

    let rec calcSeverety sum step list =
        match list with
        | [] -> sum
        | [a] -> sum + if pingpong (snd a) step = 0 then snd a * step else 0
        | h::tail -> let hit = if pingpong (snd h) step = 0 then snd h * step else 0
                     let stepDif = fst (List.head tail) - fst h
                     calcSeverety (sum + hit) (step + stepDif) tail 

    calcSeverety 0 initialDelay input             

let testList = [(0, 3); (1, 2); (4, 4); (6, 4)]

let testSeverety = testList |> getSeverety 0

let tripSeverety = input |> getSeverety 0

let findMinimalDelay =
    let rec findDelay delay input =
        if getSeverety delay input = 0 
        then delay 
        else findDelay (delay + 1) input
    findDelay 0  

let minimalDelay = input |> findMinimalDelay
