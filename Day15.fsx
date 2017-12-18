// int to array of 0 and 1
let intToBits value n =
    let getDigits count = [for i in 0..count - 1 do yield 1 <<< i]
    
    getDigits n 
    |> List.fold (fun s x -> sign(value &&& x)::s) [] 
    |> List.skipWhile (fun x -> x = 0)

let bitsToInt n bits =
    List.rev bits |> List.mapi (fun i x -> if i < n then (1 <<< i) * x else 0) |> List.sum

// bitwise shift
let (<<<&) a n = 
    let zeros = [for _ in 0..n-1 -> 0]
    List.concat [a; zeros]

//binary sum
let (++&) a b =
    let rec binSum x y trans acc =
        let bitSum x y t =
            match x + y + t with
            | 0 -> 0, 0
            | 1 -> 1, 0
            | 2 -> 0, 1
            | 3 -> 1, 1

        match x, y with
        | [], [] -> if trans = 0 then acc else trans::acc
        | h1::tail1, [] -> 
            let result, t = bitSum h1 0 trans
            binSum tail1 [] t (result::acc)
        | [], h1::tail1 -> 
            let result, t = bitSum h1 0 trans
            binSum tail1 [] t (result::acc)        
        | h1::tail1, h2::tail2 -> 
            let result, t = bitSum h1 h2 trans
            binSum tail1 tail2 t (result::acc)
    binSum (List.rev a) (List.rev b) 0 [] 
    |> List.skipWhile (fun x -> x = 0)        

let (--&) a b =
    let bitDiff x y t =
        match x - y - t with
        | 0 -> 0, 0
        | 1 -> 1, 0
        | -1 -> 1, 1
        | -2 -> 0, 1

    let rec binSum x y trans acc =
        match x, y with
        | [], [] -> if trans = 0 then acc else trans::acc
        | h1::tail1, [] -> 
            let result, t = bitDiff h1 0 trans
            binSum tail1 [] t (result::acc)
        | [], h1::tail1 -> 
            let result, t = bitDiff h1 0 trans
            binSum tail1 [] t (result::acc)        
        | h1::tail1, h2::tail2 -> 
            let result, t = bitDiff h1 h2 trans
            binSum tail1 tail2 t (result::acc)
    binSum (List.rev a) (List.rev b) 0 []   
    |> List.skipWhile (fun x -> x = 0)                 


//binary multipy
let bitMultiply value1 value2 =
    let b1 = value1
    let b2 = value2 |> List.rev
    b2 
    |> List.mapi (fun i x -> if x = 1 then b1 <<<& i else []) 
    |> List.fold ( ++& ) [] |> List.skipWhile (fun x -> x = 0)


let (>&) a b =
    if List.length a > List.length b
    then 1
    elif List.length a < List.length b
    then -1
    else
        List.compareWith (fun x y -> 
            if x > y then 1
            elif x < y then -1
            else 0) a b

let reminder a b =
    let rec getMax limit value prevValue =
        match limit >& value with
        | 1 -> getMax limit (value <<<& 1) value
        | 0 -> value
        | -1 -> prevValue

    let rec bitDivisionRec value1 value2 =
        if value1 >& value2 >= 0
        then
            let divider = getMax value1 value2 value2
            value2 |> bitDivisionRec (value1 --& divider) 
        else
            value1
    bitDivisionRec a b  


let rec checkSum n prevA prevB count sum =
    if n < count
    then 
        let currA = reminder (bitMultiply prevA factorA) div
        let currB = reminder (bitMultiply prevB factorB) div

        let sum = if (currA |> bitsToInt 16) = (currB |> bitsToInt 16) then sum + 1 else sum
        checkSum (n + 1) currA currB count sum 
    else
        sum    

let factorA = intToBits 16807 16
let factorB = intToBits 48271 16
let div = intToBits 2147483647 32

checkSum 0 (intToBits 65 16) (intToBits 8921 16) 5 0

// let x = reminder (bitMultiply (intToBits 65 32) factorA ) div

// let y = reminder (bitMultiply x factorA) div

// let z = reminder (bitMultiply y factorA) div





// b348
// getMax b348 b5 b5

// reminder (intToBits 348 16) (intToBits 5 16)
// let rec checkSum n prevA prevB count sum =
//     if n < count
//     then 
//         let currA = bitMultiply 16 prevA factorA |> bitsToInt 16
//         let currB = bitMultiply 16 prevB factorB |> bitsToInt 16
//         let currA = reminder currA div 16 |> bitsToInt 16
//         let currB = reminder currB div 16 |> bitsToInt 16
//         printfn "%d %d" currA currB
//         let sum = if currA = currB then sum + 1 else sum
//         checkSum (n + 1) currA currB count sum 
//     else
//         sum    
