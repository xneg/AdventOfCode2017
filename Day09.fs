module Day09

// problem page
// http://adventofcode.com/2017/day/9

let problemFileName = @"Data\09.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new System.IO.StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line
    };;

let line = problemFileName |> processFile |> Seq.find (fun _ -> true)

let explode (s:string) =
        [for c in s -> c]

let (|CurrentLetter|) list = List.head list

let rec myFunc list (ignore : bool) =
    let tail = List.tail list
    match list with
    | [_] -> printfn "finish"
    | CurrentLetter '{' -> 
        printf "{" 
        myFunc tail ignore
    | CurrentLetter '}' -> 
        printf "}" 
        myFunc tail ignore
    | CurrentLetter '<' -> 
        printf "<"
        myFunc tail ignore
    | CurrentLetter '>' -> 
        printf ">"
        myFunc tail ignore
    | CurrentLetter '!' -> 
        printf "!"
        myFunc tail (not ignore)        
    | _ -> myFunc tail ignore

let testInput = "{<a>,<a>,<a>,<a>}"

testInput |> explode |> myFunc

let x: (char list) = ['a']

List.tail x

// let (|ToBool|_|) x =
// let success, result = Boolean.TryParse(x)
// if success then Some(result)
// else None
// let (|ToInt|_|) x =
// let success, result = Int32.TryParse(x)
// if success then Some(result)
// else None
// let (|ToFloat|_|) x =
// let success, result = Double.TryParse(x)
// if success then Some(result)
// else None
// let describeString str =
// match str with
// | ToBool b -> printfn “%s is a bool with value %b” str b
// | ToInt i -> printfn “%s is an integer with value %d” str i
// | ToFloat f -> printfn “%s is a float with value %f” str f
// | _ -> printfn “%s is not a bool, int, or float” str