// module Day23

open System.Collections.Generic
open System.IO
let problemFileName = @"Data\23.txt"
let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split([|' '|])
    };;

let instructions = problemFileName
                |> processFile
                |> Seq.toArray

let registers = new Dictionary<string, int>() 

registers.Clear()
for i in 'a'..'h' do
    registers.Add(i.ToString(), 0)  

let getValue x = 
    if not <| registers.ContainsKey x then registers.Add(x, (int)x)
    registers.[x]

let mutable currentPosition = 0;
let mutable mulCount = 0;

let set x y = registers.[x] <- y
let sub x y = registers.[x] <- registers.[x] - y
let mul x y = 
    registers.[x] <- registers.[x] * y
    mulCount <- mulCount + 1
let jnz x y = if getValue x <> 0 then y else 1

let doInstruction instruction = 
    match instruction with 
    | [|a;x;c|] ->  if a = "set" then 
                        set x <| getValue c
                        1
                    elif a = "sub" then
                        sub x <| getValue c
                        1   
                    elif a = "mul" then
                        mul x <| getValue c
                        1
                    elif a = "jnz" then
                        jnz x (getValue c)
                    else 0

registers.Clear()
for i in 'a'..'h' do
    registers.Add(i.ToString(), 0)
currentPosition <- 0

// for i in 0..1000 do
//     currentPosition <- currentPosition + doInstruction instructions.[currentPosition]                           
let rec performAction (instructions : string[][]) = 
    if currentPosition < instructions.Length 
    then 
        currentPosition <- currentPosition + doInstruction instructions.[currentPosition]
        performAction instructions
    else
        ()    
let getResult1 =
    registers.Clear()
    for i in 'a'..'h' do
        registers.Add(i.ToString(), 0)
    currentPosition <- 0
    mulCount <- 0
    performAction instructions
    mulCount     

let instructions' = @"Data\23_1.txt"
                   |> processFile
                   |> Seq.toArray
let getResult1_1 =
    registers.Clear()
    registers.Add("b", 84)
    registers.Add("c", 84)
    for i in 'd'..'h' do
        registers.Add(i.ToString(), 0)
    currentPosition <- 0
    mulCount <- 0
    performAction instructions'
    mulCount

(* Prepare for snd part *)
registers.Clear()
registers.Add("a", 1)
for i in 'b'..'h' do
    registers.Add(i.ToString(), 0)
currentPosition <- 0
mulCount <- 0

let snapshot () =
    ("pos", currentPosition)::[for kvp in registers -> (kvp.Key, kvp.Value)]

let rec waitForHChange () =
    currentPosition <- currentPosition + doInstruction instructions.[currentPosition]
    if registers.["h"] = 0 then waitForHChange ()

waitForHChange ()

//snd part
let rec getPrimes = function
      [] -> []
    | h::t -> h::getPrimes (t |>List.filter (fun x -> x%h > 0))

let primes = getPrimes [2..108400+ 17000]

let input = [108400..17..108400+17000]

let totalPrimes = Set.intersect (Set.ofList primes) (Set.ofList input) |> Seq.length

let result = input.Length - totalPrimes