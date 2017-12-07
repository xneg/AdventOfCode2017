module Day07

// problem page
// http://adventofcode.com/2017/day/7

open Utilities
open System.Text.RegularExpressions
open System.Collections.Generic

let problemFileName = @"Data\07.txt"

let lines = problemFileName |> Helper.processFile 

type Program = string * int 

type Tree = 
    | Branch of Program * Tree list
    | Leaf of Program

let func (x: string[]) : Program = ("dfdf", 1)

let leafs = lines |> Seq.map (fun x -> x.[3..x.Length - 1])

let diskSpace (s : string) =
    (int)s.[1..s.Length - 2]

let toProgram (x : string[]) : Program = (x.[0], diskSpace x.[1])

let programs = new Dictionary<string, Program>

Seq.iter (fun (x : string[]) -> printf "%s" x.[0]) [[|"fjkfpm"; "(69)"; "->"; "kohxzh,"; "liwvq,"; "eqkio,"; "xvoyybs"|];
     [|"dsiixv"; "(52)"|]; [|"fhimhm"; "(66)"|]; [|"mdlubuq"; "(73)"|]]

// let nodes2 = lines |> Seq.map (fun x -> Program(x.[0], 1)) //как так??
// leafs

// nodes

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