module Day06

// problem page
// http://adventofcode.com/2017/day/6

let blocks = [4;10;4;1;8;4;9;14;5;1;14;15;0;15;3;5]

let getMaxElem list =
    list
    |> List.mapi (fun i v -> i, v)
    |> List.maxBy snd

let valueChange value index length =
    value / length + sign ( (value % length) / index)

let circleIndex pivotIndex index length =
    let diff = index - pivotIndex
    if (diff >= 0) then diff else length + diff

let changed blocks = 
    let blocksLength = List.length blocks
    let pivotIndex, pivotIndexVal = getMaxElem blocks
    blocks 
    |> List.mapi (fun i value -> 
        let newI = circleIndex pivotIndex i blocksLength
        if newI = 0 
        then pivotIndexVal / blocksLength
        else value + valueChange pivotIndexVal newI blocksLength)

let rec calcSteps blocks n hashTable =
    let blocks = changed blocks
    let h = hash blocks
    if List.contains h hashTable
    then n + 1, blocks, List.length hashTable - List.findIndex (fun x -> x = h) hashTable
    else calcSteps blocks (n + 1) (h::hashTable)

let result blocks =
    calcSteps blocks 0 List.empty

result blocks

result [0;2;7;0]
// let h = (changed >> changed >> changed) blocks |> hash

12841 - 4803;;