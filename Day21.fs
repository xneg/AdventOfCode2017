module Day21

// problem page
// http://adventofcode.com/2017/day/21

open System.Text.RegularExpressions

let processFile (filePath : string) =
        seq {
            use fileReader = new System.IO.StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let problemFileName = @"Data\21.txt"

let getMatrix (str : string) =
    let explode (s:string) =
        [|for c in s -> c|]
    let x = str.Split('/') |> Array.map (explode)
    Array2D.init x.Length x.[0].Length (fun i j -> x.[i].[j])

let rotate (matrix : char[,]) = 
    let d1, d2 = Array2D.length1 matrix, Array2D.length2 matrix
    Array2D.init d1 d2 (fun i j -> matrix.[d2 - j - 1, i])

let flipH (matrix : char[,]) =
    let d1, d2 = Array2D.length1 matrix, Array2D.length2 matrix
    Array2D.init d1 d2 (fun i j -> matrix.[i, d2 - j - 1])

let flipV (matrix : char[,]) =
    let d1, d2 = Array2D.length1 matrix, Array2D.length2 matrix
    Array2D.init d1 d2 (fun i j -> matrix.[d1 - i - 1, j])        

let getAll (matrix : char[,]) =
    let r1 = rotate matrix
    let r2 = matrix |> rotate |> rotate
    let r3 = matrix |> rotate |> rotate |> rotate
    [matrix; flipH matrix; flipV matrix; r1; flipH r1; flipV r1; r2; flipH r2; flipV r2; r3; flipH r3; flipV r3]

let toPattern str =
    let m = Regex.Match(str, @"^(.*?) => (.*)")
    m.Groups.[1].Value |> getMatrix |> getAll, m.Groups.[2].Value |> getMatrix

let patternsBook = problemFileName |> processFile |> Seq.map (toPattern) |> Seq.toList

let initial = ".#./..#/###" |> getMatrix

let divide size (pattern: char[,]) =
    let len = Array2D.length2 pattern
    [|for i in 0..len / size - 1 do
        for j in 0..len / size - 1 do
            yield pattern.[i * size..(i + 1) * size - 1,j * size..(j + 1) * size - 1]|]

let z = ".#.#/..../.#.#/#.##" |> getMatrix |> divide 2

let concat (patterns : char[,] []) =
    let sqLength = sqrt (float patterns.Length) |> int 
    let small = Array2D.length1 patterns.[0]
    let finalSize = sqLength * small
    let getValue i j =
        let I, J = i / small, j /small
        let pInd = I * sqLength + J 
        patterns.[pInd].[i - I * small, j - J * small]
    Array2D.init finalSize finalSize (getValue)

let mutate (pattern: char[,]) =
    patternsBook 
    |> List.find (fun x -> fst x |> List.contains pattern)
    |> snd

// let toStr pattern =
//     pattern
//     |> Seq.cast<char> 
//     |> Seq.toList 
//     |> List.mapi (fun i x -> if i % 3 = 2 && i <> 8 then string x + "/" else string x)
//     |> List.reduce ( + )

let grow (pattern: char[,]) =
    let size = if Array2D.length1 pattern % 2 = 0 then 2 else 3
    pattern |> divide size |> Array.map mutate |> concat

let final = initial |> grow |> grow |> grow |> grow |> grow

let result = final |> Seq.cast<char> |> Seq.filter (fun x -> x = '#') |> Seq.length

let rec growNTimes n finish pattern =
    if n < finish then
        pattern |> grow |> growNTimes (n + 1) finish
    else
        pattern 

let final2 = initial |> growNTimes 0 18       

let result2 = final2 |> Seq.cast<char> |> Seq.filter (fun x -> x = '#') |> Seq.length