// module Duet

// problem page
// http://adventofcode.com/2017/day/18

open System.Collections.Generic
open System.IO

let problemFileName = @"Data\18.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split([|' '|])
    };;

type Singer (instructions : string[][], funcWrite, funcGet) =
    
    let registers = new Dictionary<string, bigint>()
    let mutable currIns = 0;
    let mutable locked = false;

    let getValue x = if registers.ContainsKey x then registers.[x] else 0I
    let snd = funcWrite 
    let set x value = if registers.ContainsKey x then registers.[x] <- value else registers.Add (x, value)
    let add x value = set x (getValue x + value)
    let mul x value = set x (getValue x * value)
    let _mod x value = set x (getValue x % value)
    
    let rcv x = 
        let v : bigint option = funcGet() 
        if v.IsSome then
            // printfn "%A" v.Value
            set x v.Value
        else
            // printfn "locked"  
            locked <- true 
            currIns <- currIns - 1          
    let jgz x value = currIns <- currIns + if x > 0I then value - 1 else 0

    let doInstruction instruction = 
        let getIntValue str =
            match System.Int32.TryParse str with
            | true, num -> System.Numerics.BigInteger num
            | _ -> getValue str

        match instruction with 
        | [|a;x|] when a = "snd" -> getIntValue x |> snd
        | [|a;x|] when a = "rcv" -> rcv x 
        | [|a;x;c|] ->  let value = getIntValue c
                        if a = "set" then set x value
                        elif a = "add" then add x value
                        elif a = "mul" then mul x value
                        elif a = "mod" then _mod x value
                        elif a = "jgz" then jgz (getIntValue x) ((int)value)

    let rec playSounds () =
        if currIns < instructions.Length then
            doInstruction instructions.[currIns]
            currIns <- currIns + 1 
            playSounds ()
        else
            ()        
    
    member this.PlaySouds () =
        currIns <- 0
        registers.Clear () |> ignore  
        playSounds ()  

    member this.PlaySound() =
        locked <- false
        if currIns < instructions.Length then
            doInstruction instructions.[currIns]
            currIns <- currIns + 1
        else
            locked <- true            

    member this.Registers = registers 
    member this.Locked = locked   
    member this.CurrentIns = currIns                        

let instructions = problemFileName
                |> processFile
                |> Seq.toArray

let get queue = 
    match List.rev queue with
    | [] -> None, List.empty
    | [r] -> 
            Some(r), List.empty
    | h::t -> 
            Some(h), List.rev t        

let mutable queue0 = List.empty

let mutable queue1 = List.empty

let mutable result = 0

let write0 value = 
    queue0 <- (value::queue0) 
    result <- result + 1

let write1 value = queue1 <- (value::queue1)

let get0 () =
    let r, q = get queue0
    queue0 <- q
    r

let get1 () =
    let r, q = get queue1
    queue1 <- q
    r

let fstSinger = Singer(instructions, write1, get0)
fstSinger.Registers.Add("p", 0I)

let sndSinger = Singer(instructions, write0, get1)
sndSinger.Registers.Add("p", 1I)

let rec duet () =
    if not fstSinger.Locked || not sndSinger.Locked then
        fstSinger.PlaySound()
        sndSinger.PlaySound()
        duet ()

duet ()   

result
