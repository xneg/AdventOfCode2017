module Day24

// problem page
// http://adventofcode.com/2017/day/20

open System.IO

let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line.Split('/')
        }

let problemFileName = @"Data\24.txt" 

let components = problemFileName 
                |> processFile 
                |> Seq.map (fun x -> (int(x.[0]), int(x.[1])))  
                |> Seq.toList                

let rec createBridge currentPort available currentBridge =
    let getEmptyPort x =
        if fst x = currentPort then snd x else fst x

    let getOther a = available |> List.filter (fun x -> x <> a)    

    let a = available |> List.filter (fun x -> fst x = currentPort || snd x = currentPort)
    
    match a with
    | [] -> [currentBridge]
    | _ -> a |> List.fold (fun s x -> createBridge (getEmptyPort x) (getOther x) (x::currentBridge) @ s) []


let bridges = createBridge 0 components []

let strongest = bridges |> List.map (fun x -> x |> List.fold (fun s x -> s + fst x + snd x) 0) |> List.max

//2nd part

let maxLength = bridges |> List.map (fun x -> x.Length) |> List.max
let longestAndStronges = bridges 
                        |> List.filter (fun x -> x.Length = maxLength) 
                        |> List.map (fun x -> x |> List.fold (fun s x -> s + fst x + snd x) 0) 
                        |> List.max
