module Day16

// problem page
// http://adventofcode.com/2017/day/16

open System.Text.RegularExpressions
open System.IO

let spin x (s : string) = s.[s.Length - x..s.Length - 1] + s.[0..s.Length - x - 1]

let exchange a b (s: string) = 
    let arr = [for c in s -> c]
    arr 
    |> List.mapi (fun i x -> 
                        if i = a then arr.[b] 
                        elif i = b then arr.[a]
                        else x)
    |> List.fold (fun s x -> s + x.ToString()) ""     

let partner a b (s: string) =     
    let arr = [for c in s -> c]
    arr 
    |> List.map (fun x -> 
                        if x = a then b 
                        elif x = b then a
                        else x)
    |> List.fold (fun s x -> s + x.ToString()) ""

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let proceed instruction =
    match instruction with
    | Regex @"s(\d+)" [x] -> spin (int x)
    | Regex @"x(\d+)\/(\d+)" [a; b] -> exchange (int a) (int b)
    | Regex @"p(\w)\/(\w)" [a; b] -> partner a.[0] b.[0]

// "abcde" |> proceed "s1" |> proceed "x3/4" |> proceed "pe/b"

let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line.Split ','
        }

let problemFileName = @"Data\16.txt"

let dancers =
    [for i in 'a'..'p' -> i] |> List.fold (fun s x -> s + x.ToString()) ""

let moves = problemFileName 
            |> processFile
            |> Seq.head

let performDance = Array.fold (fun s x -> proceed x s)

let result1 = performDance dancers moves  

// 2nd part
let rec performInfiniteDance n finish position =
    if (position = dancers && n <> 0) || n = finish then
        n, position
    else
        performDance position moves |> performInfiniteDance (n + 1) finish  

let cycle = fst (performInfiniteDance 0 1000000000 dancers)

let result2 = dancers |> performInfiniteDance 0 (1000000000 % cycle) |> snd
