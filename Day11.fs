module Day11

// problem page
// http://adventofcode.com/2017/day/11

let problemFileName = @"Data\11.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new System.IO.StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split([|','|])
    };;

let direction = function
    | "n" -> (0, 1, -1)
    | "s" -> (0, -1, 1)
    | "ne" -> (1, 0, -1)
    | "se" -> (1, -1, 0)
    | "nw" -> (-1, 1, 0)
    | "sw" -> (-1, 0, 1)

let (+++) x y = 
    let x1, x2, x3 = x
    let y1, y2, y3 = y
    (x1 + y1, x2 + y2, x3 + y3)

let dist x =
    let x1, x2, x3 = x
    (abs(x1) + abs(x2) + abs(x3)) / 2

let finalPosition = 
    problemFileName 
    |> processFile 
    |> Seq.concat  
    |> Seq.map(direction) 
    |> Seq.fold ( +++ ) (0, 0, 0)

let currentDistance = dist finalPosition

let rec getDistances currentPos agg dirList =
    match dirList with
    | [] -> agg
    | h :: tail -> let newPos = currentPos +++ h
                   getDistances newPos (dist newPos :: agg) tail

let maxDistance =
    problemFileName 
    |> processFile 
    |> Seq.concat  
    |> Seq.map(direction) 
    |> Seq.toList    
    |> getDistances (0, 0, 0) [] 
    |> List.max 