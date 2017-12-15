module Day14

// problem page
// http://adventofcode.com/2017/day/14

open Utilities
open System

let input = "hfdlxzhv"

let generateGrid input =
    [for i in 0 .. 127 do yield sprintf "%s-%d" input i]

let grid = input |> generateGrid

let hexToByte hex = 
    Convert.ToString(Convert.ToInt32(hex.ToString(), 16), 2).PadLeft(4, '0')

let convertToBytes = Helper.explode >> List.map hexToByte >> String.concat System.String.Empty 

let countOnes = convertToBytes >> Helper.explode >> List.filter (fun x -> x = '1') >> List.length

grid 
|> List.map (Helper.knotHash >> countOnes) |> List.sum

let zerosAndOnes = grid |> List.map (Helper.knotHash >> convertToBytes)

let map = zerosAndOnes |> List.map (Helper.explode >> List.toArray) |> List.toArray

let getNeighbours i j (map : char[][]) =
        let vert =  [for i' in (max (i - 1) 0)..(min (i + 1) (map.Length - 1)) do
                        if i <> i' then yield map.[i'].[j], i' * map.Length + j]

        let hor = [for j' in (max (j - 1) 0)..(min (j + 1) (map.[i].Length - 1)) do
                    if j <> j' then yield map.[i].[j'], i * map.Length + j']  

        List.concat [vert; hor]       
                 

// let map = [[|0;2;3|];[|4;5;6|];[|7;8;9|]] |> List.toArray

let graph = map |> Array.mapi (
            fun i x -> x 
                    |> Array.mapi (
                        fun j x ->  
                                    if (x = '0') 
                                    then [||] 
                                    else getNeighbours i j map |> List.filter (fun x -> fst x <> '0') |> List.map snd |> List.toArray))            
                |> Array.fold (fun acc x -> Array.concat [acc; x]) [||]      

let data = graph

let createGroups =
    let rec sendMessage mark ips acc initial  =
        let fsts = List.map (fst)
        match ips with
        | [] -> acc, initial
        | _ -> 
            let receivers = ips 
                            |> List.collect (fun x -> data.[x] |> Array.toList) 
                            |> List.filter (fun x -> not <| List.contains x (fsts acc))
            if List.isEmpty receivers 
            then acc, initial    
            else                                 
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

            






