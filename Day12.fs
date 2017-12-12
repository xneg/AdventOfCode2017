module Day12

// problem page
// http://adventofcode.com/2017/day/12

open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
let problemFileName = @"Data\12.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line
    };;

let lines file = 
    file 
    |> processFile 
    |> Seq.map (fun x -> Regex.Split (x, @"\W") 
                        |> Array.filter (fun x -> x <> "") 
                        |> Array.map (fun x -> (int)x))
    |> Seq.toList

let append list array =
    Array.toList array @ list

let data = new Dictionary<int, int[]>()

let fillData = List.iter (fun (x : int[]) -> data.Add(x.[0], x.[1..]))

lines problemFileName |> fillData

let ipsDic = new Dictionary<int, int>()

data.Keys |> Seq.iter (fun x -> ipsDic.Add(x, 0))


let rec sendMessage mark ips  =
    match ips with
    | [] -> ()
    | _ -> 
        let receivers = ips |> List.collect (fun x -> data.[x] |> Array.toList) |> List.filter (fun x -> ipsDic.[x] = 0)
        receivers |> List.iter (fun x -> ipsDic.[x] <- mark)        
        sendMessage mark receivers 

sendMessage 1 [0]            
        
let x = ipsDic.Values |> Seq.filter (fun x -> x = 1) |> Seq.sum

data.Keys |> Seq.iter (fun x -> ipsDic.[x] <- 0)


let findEmptyKey () = ipsDic.Keys |> Seq.tryFind (fun x -> ipsDic.[x] = 0)

let rec calcGroups ip mark =
    match ip with
    | Some(a) -> sendMessage mark [a]
                 calcGroups (findEmptyKey ()) (mark + 1)
    | None -> ()        

calcGroups (Some 0) 1     

ipsDic.Values |> Seq.distinct |> Seq.length

ipsDic.Values |> Seq.max