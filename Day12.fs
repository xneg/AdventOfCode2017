module Day12

// problem page
// http://adventofcode.com/2017/day/12

open System.IO
open System.Text.RegularExpressions
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

let data = lines problemFileName |> List.map (fun x -> x.[1..]) |> List.toArray         

let createGroups =
    let rec sendMessage mark ips acc initial  =
        let fsts = List.map (fst)
        match ips with
        | [] -> acc, initial
        | _ -> 
            let receivers = ips 
                            |> List.collect (fun x -> data.[x] |> Array.toList) 
                            |> List.filter (fun x -> not <| List.contains x (fsts acc))
            let reduced = initial |> List.except receivers                    
            let newAcc = receivers |> List.fold (fun s x -> (x, mark)::s) acc      
            sendMessage mark receivers newAcc reduced

    let rec calcGroupsRec ip mark acc initial =
        let findEmptyKey acc =
            let fsts = List.map (fst)
            initial |> List.tryFind (fun x -> not <| List.contains x (fsts acc))
        match ip with
        | Some(a) -> let newAcc, reduced = sendMessage mark [a] ((a, mark)::acc) initial
                     calcGroupsRec (findEmptyKey newAcc) (mark + 1) newAcc reduced
        | None -> acc 

    [0..data.Length - 1] |> calcGroupsRec (Some 0) 1 []    

let groups = createGroups

let zeroGroupMembers = groups |> List.filter (fun x -> snd x = 1) |> List.length
let groupsCount = groups |> List.maxBy snd |> snd

