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

let rec calcBlocks score blocks str =
    let tail = List.tail str
    match str with
    | [_] -> score :: blocks
    | CurrentLetter '{' -> calcBlocks (score + 1) blocks tail
    | CurrentLetter '}' -> calcBlocks (score - 1) (score :: blocks) tail
    | _ -> calcBlocks score blocks tail

let rec calcBlocksWithGarbage score blocks isGarbage str  =
    let tail = List.tail str
    match str with
    | [_] -> score :: blocks
    | CurrentLetter '{' -> if not isGarbage 
                            then calcBlocksWithGarbage (score + 1) blocks isGarbage tail 
                            else calcBlocksWithGarbage score blocks isGarbage tail
    | CurrentLetter '}' -> if not isGarbage 
                            then calcBlocksWithGarbage (score - 1) (score :: blocks) isGarbage tail 
                            else calcBlocksWithGarbage score blocks isGarbage tail
    | CurrentLetter '<' -> calcBlocksWithGarbage score blocks true tail
    | CurrentLetter '>' -> calcBlocksWithGarbage score blocks false tail
    | CurrentLetter '!' -> if isGarbage 
                            then calcBlocksWithGarbage score blocks isGarbage (List.tail tail)
                            else calcBlocksWithGarbage score blocks isGarbage tail
    | _ -> calcBlocksWithGarbage score blocks isGarbage tail

let testInput = "{{<!>},{<!>},{<!>},{<a>}}"

testInput |> explode |> calcBlocksWithGarbage 0 [] false |> List.sum

line |> explode |> calcBlocksWithGarbage 0 [] false |> List.sum

//2nd part
let rec calcGarbage symbols isGarbage str  =
    let tail = List.tail str
    match str with
    | [_] -> symbols
    | CurrentLetter '<' -> if isGarbage then calcGarbage (symbols + 1) true tail else calcGarbage symbols true tail
    | CurrentLetter '>' -> calcGarbage symbols false tail
    | CurrentLetter '!' -> if isGarbage 
                            then calcGarbage symbols isGarbage (List.tail tail)
                            else calcGarbage symbols isGarbage tail
    | _ -> if isGarbage then calcGarbage (symbols + 1) true tail else calcGarbage symbols false tail

line |> explode |> calcGarbage 0 false


