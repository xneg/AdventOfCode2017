module Day10

// problem page
// http://adventofcode.com/2017/day/10

let list = [|0..255|]//[|0..4|]//[|0..255|]

let inputLengths = [83; 0; 193; 1 ; 254; 237; 187; 40; 88; 27; 2; 255; 149; 29; 42; 100]


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
        //(Array.concat [|changed.[ll-curPos..]; changed.[0..ll-curPos-1]|], skip + 1, (curPos + length) % ll)

let testResult = [3; 4; 1; 5] |> getHash 0 0 [|0..4|]

let result = inputLengths |> getHash 0 0 [|0..255|]

//92 * 218 = 20056

// 2nd part

let explode (s:string) =
        [for c in s -> c]

let input = "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100" //"AoC 2017"

let roundSequence = (explode input |> List.map (fun c -> (int)c)) @ [17; 31; 73; 47; 23]  

let totalSequence = [1..64] |> List.fold (fun s _ -> s @ roundSequence) []

let sparseHash = totalSequence |> getHash 0 0 [|0..255|]

let testArray = [|65 ; 27 ; 9 ; 1 ; 4 ; 3 ; 40 ; 50 ; 91 ; 7 ; 6 ; 0 ; 2 ; 5 ; 68 ; 22|]
let calcXOR (list : int[]) = Array.fold (( ^^^ )) list.[0] list.[1..]

let denseHash = [for i in [0..15] do yield (calcXOR sparseHash.[i * 16.. i * 16 + 15])]

denseHash |> List.map (fun x -> System.String.Format("{0:x2}", x)) |> String.concat System.String.Empty

// d9a7de4a809c56bf3a9465cb84392c8e