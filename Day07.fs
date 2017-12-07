module Day07

// problem page
// http://adventofcode.com/2017/day/7

open Utilities
open System.Text.RegularExpressions

let problemFileName = @"Data\07.txt"

let x = problemFileName |> Helper.processFile 

type Leaf = string * int 

// type Tree = 
//     | Branch of Entity * Tree list
//     | Leaf of Entity

let func (x: string[]) = Leaf("dfdf", 1)
let x : Leaf = ("sdfs", 1)

let leafs = x |> Seq.map (fun x -> x.[3..x.Length - 1])

let diskSpace (s : string) =
    (int)s.[1..s.Length - 2]

let myFunc (x : string[]) = Program(x.[0], (int)x.[1])

// let nodes = x |> Seq.map (fun _ -> Program)

leafs

nodes

// let (|RegexMatchLeaf|_|) (input : string) =
//     let result = Regex.Match(input, @"(.*?) \((.*?)\)")
//     if result.Success then
//         Some(Leaf(result.Groups.[0].Value, (int)result.Groups.[1].Value))
//     else
//         None

// let (|RegexMatchBranch|_|) (input : string) =
//     let result =Regex.Match(input, @"(.*?) \((.*?)\) (->) (.*)")
//     if result.Success then
//         Some(Branch(result.Groups.[0].Value, (int)result.Groups.[1].Value, list.Empty))
//     else
//         None    