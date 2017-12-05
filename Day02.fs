module Day02

// problem page
// http://adventofcode.com/2017/day/2

open Utilities

let testFileName = @"Data\02_test.txt"
let problemFileName = @"Data\02.txt"

let lineCheckSumGeneral f l =
    let intLine = Array.map (fun (x : string) -> (int)x) l
    f intLine

let fstAlg line = Array.max line - Array.min line

let checkSum fileName (alg : int[] -> int) =
    Seq.sumBy (lineCheckSumGeneral alg) (Helper.processFile fileName) 

// let evenlyDivisible x y = if (y % x = 0) then Some(y/x) else None

// let evenlyDivisibleArray a x =
//     a 
//     |> Seq.map (evenlyDivisible x)
//     |> Seq.choose id
//     |> Seq.sum

let evenlyDivisibleArray a x =
    let evenlyDivisible x y = if (y % x = 0) then y/x else 0 
    Seq.sumBy (evenlyDivisible x) a

let arrayCheckSum2 a =
    (a
    |> Seq.map (evenlyDivisibleArray a)
    |> Seq.filter(fun x -> x > 1)
    |> Seq.sum) - 1

let sndAlg line = arrayCheckSum2 line

let testResult1 = checkSum testFileName fstAlg;;
let result1 = checkSum problemFileName fstAlg;;

let testResult2 = checkSum testFileName sndAlg;;
let result2 = checkSum problemFileName sndAlg;;