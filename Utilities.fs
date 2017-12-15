namespace Utilities

module Helper =
    open System.IO

    let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line.Split([|'\t';' '|])
        }

    let explode (s:string) =
        [for c in s -> c]
    let knotHash (str) =
        let rec getHash pos skip (cycleList : int[]) (input : int list) =
            match input with
            | [] -> cycleList
            | h :: tail ->
                let length = h
                let ll = cycleList.Length
                let curPos = pos % ll
                let newArray = Array.concat [|cycleList.[curPos..]; cycleList.[0..curPos - 1]|]
                let changed = Array.concat [|Array.rev newArray.[0..length - 1]; newArray.[length..]|]
                let final = Array.concat [|changed.[ll-curPos..]; changed.[0..ll-curPos-1]|]
                getHash ((curPos + length + skip) % ll) (skip + 1) final tail

        let calcXOR (list : int[]) = Array.fold (( ^^^ )) list.[0] list.[1..]
        let roundSequence = List.append (str |> explode |> List.map (fun c -> (int)c)) [17; 31; 73; 47; 23]
        let sparseHash = [1..64] |> List.fold (fun s _ -> s @ roundSequence) [] |> getHash 0 0 [|0..255|]
        [for i in [0..15] do yield (calcXOR sparseHash.[i * 16.. i * 16 + 15])]
        |> List.map (fun x -> System.String.Format("{0:x2}", x)) 
        |> String.concat System.String.Empty    
