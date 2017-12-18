module Day17

// problem page
// http://adventofcode.com/2017/day/17
let rec insert input pos value =
    match pos, input with
    | 0, h -> value::h
    | pos, h::tail -> h::insert tail (pos - 1) value


let rec createSpinlock list currPos value cycleCount =
    let step = 356
    let spinlock list currPos value =
        let insPosition = (currPos + step) % List.length list + 1
        insert list insPosition value, insPosition

    if value <= cycleCount
    then 
        let list, currPos = spinlock list currPos value
        createSpinlock list currPos (value + 1) cycleCount
    else
        list    

let spinlock1 = createSpinlock [0] 0 1 2017

let i2017 = spinlock1 |> List.findIndex (fun x -> x = 2017)
let result1 = spinlock1 |> List.item (i2017 + 1)

let rec findSecondElem currPos value cycleCount result =
    let step = 356
    let insPosition = (currPos + step) % value + 1
    let result = if insPosition = 1 then value else result
    if value <= cycleCount
    then
        findSecondElem insPosition (value + 1) cycleCount result
    else
        result    

let result2 = findSecondElem 0 1 50000000 0
