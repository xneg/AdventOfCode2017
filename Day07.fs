module Day07

// problem page
// http://adventofcode.com/2017/day/7

open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO

let problemFileName = @"Data\07.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line
    };;

let lines = problemFileName |> processFile

type Program = { Name : string; Weight : int } 

type Tree = 
    | Branch of Program * Tree list
    | Leaf of Program

let leafs = lines |> Seq.map (fun x -> x.[3..x.Length - 1])

let toProgram (x : string[]) = {Name = x.[0]; Weight = (int)x.[1] } 

let programs = new Dictionary<string, Program * string[]>()

lines 
|> Seq.map 
    (fun x -> 
        Regex.Split (x, @"\W") 
        |> Array.filter (fun x -> x <> "")) 
|> Seq.iter
    (fun (a : string[]) ->
        programs.Add(a.[0], (toProgram a, a.[2..])))        

let rec getTree name =
    let found, value = programs.TryGetValue name
    if Array.isEmpty (snd value) 
    then Leaf(fst value)
    else 
    let leafs = (snd value) |> Array.map (getTree) |> Array.toList
    Branch(fst value, leafs)
let allNodeNames = programs.Values |> Seq.map (snd) |> Seq.fold (fun s x -> Array.concat [|s; x|]) [||]
let bottom = programs.Keys |> Seq.find (fun x -> not (Array.contains x allNodeNames))

//2nd part

let tree = getTree "cqmvs"

let rec getWeight node =
    match node with
    | Branch (program, list) -> List.fold (fun s x -> s + getWeight x) program.Weight list
    | Leaf(program) -> program.Weight

let findBadNode treeList =
    let badNode =
        treeList
        |> List.distinctBy (getWeight)
        |> List.tryFind (fun _ -> true)

    let badWeight = getWeight badNode.Value
    let reference = (List.fold (fun s x -> s + getWeight x) 0 treeList - badWeight) / (List.length treeList - 1)
    if (reference <> badWeight) then
        Some(badNode.Value, reference - badWeight)
    else
        None    

let rec findBadNodeRec tree diff =
    match tree with
    | Branch (program, list) -> 
        let result = findBadNode list
        if result.IsSome then 
            let badNode, newDiff = result.Value
            printfn "%d" newDiff
            findBadNodeRec badNode newDiff
        else
            program.Weight + diff
    | Leaf (program) -> program.Weight + diff

let properWeight = findBadNodeRec tree 0;;
