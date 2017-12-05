module Main

open Day04

[<EntryPoint>]
let main _ =
    result1 |> printfn "%d"
    result2 |> printfn "%d"    
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code