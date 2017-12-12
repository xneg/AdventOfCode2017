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

let data = new Dictionary<int, int[]>()

let fillData = List.iter (fun (x : int[]) -> data.Add(x.[0], x.[1..]))

lines problemFileName |> fillData

let rec sendMessage mark ips acc  =
    let fsts = List.map (fst)
    match ips with
    | [] -> acc
    | _ -> 
        let receivers = ips |> List.collect (fun x -> data.[x] |> Array.toList) |> List.filter (fun x -> not <| List.contains x (fsts acc))
        let newAcc = receivers |> List.fold (fun s x -> (x, mark)::s) acc      
        sendMessage mark receivers newAcc

let rec calcGroups ip mark acc =
    let findEmptyKey acc =
        let fsts = List.map (fst)
        data.Keys |> Seq.tryFind (fun x -> not <| List.contains x (fsts acc))
    match ip with
    | Some(a) -> let newAcc = sendMessage mark [a] ((a, mark)::acc)
                 calcGroups (findEmptyKey newAcc) (mark + 1) newAcc
    | None -> acc 

let groups = calcGroups (Some 0) 1 [] 

let zeroGroupMembers = groups |> List.filter (fun x -> snd x = 1) |> List.length
let groupsCount = groups |> List.maxBy snd |> snd

