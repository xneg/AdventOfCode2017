module Day04

// problem page
// http://adventofcode.com/2017/day/4

open System.IO
let problemFileName = @"F:\Workshop\Visual Studio\FSharp\VSCode\AdventOfCode2017\Data\04.txt"
let testFileName = @"F:\Workshop\Visual Studio\FSharp\VSCode\AdventOfCode2017\Data\04_test.txt"

// Same as in Day02
let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line.Split([|'\t';' '|])
        };;

// Generic function using f as hash func
let countWords f (s: string[]) =
    s |>
    Array.fold (fun mp c -> 
        let hash = f c
        if Map.containsKey hash mp then Map.add hash (mp.[hash] + 1) mp else Map.add hash 1 mp)
        Map.empty

// Result using f as hash func
let result f fileName =
    let lines = fileName |> processFile 
    Seq.length lines -
    (lines
    |> Seq.map (countWords f)
    |> Seq.sumBy (Map.fold (fun state _ value -> if value > 1 then 1 else state) 0))

let testResult1 = result (id) testFileName;;
let result1 = result (id) problemFileName;;


//2nd part. Using prime numbers mapping to find anagram hash
let isPrime n =
  let sqrt' = (float >> sqrt >> int) n // square root of integer
  [ 2 .. sqrt' ] // all numbers from 2 to sqrt'
  |> List.forall (fun x -> n % x <> 0) // no divisors

let allPrimes =
  let rec allPrimes' n =
    seq { // sequences are lazy, so we can make them infinite
      if isPrime n then
        yield n
      yield! allPrimes' (n+1) // recursing
    }
  allPrimes' 2 // starting from 2

// We need 26 prime numbers as in english alphabet

let complicatedHash (s : string) =

    let primes = allPrimes |> Seq.take 26 |> Array.ofSeq

    let mapToPrimes (c: char) = primes.[int(c) - int('a')]

    [for c in s -> c] |> List.fold (fun acc x -> acc * mapToPrimes x) 1

let result2 = result complicatedHash problemFileName;;
