// int to array of 0 and 1
let intToBits value n =
    let getDigits count = [for i in 0..count - 1 do yield 1 <<< i]
    
    getDigits n |> List.fold (fun s x -> sign(value &&& x)::s) []

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

[0;1;1] ++& [1]

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

[1;1;0;1] --& [1;1;0]

//binary multipy
let bitMultiply n value1 value2 =
    let b1 = intToBits value1 n
    let b2 = intToBits value2 n |> List.rev
    b2 |> List.mapi (fun i x -> if x = 1 then b1 <<<& i else []) |> List.fold ( ++& ) []

bitMultiply 16 16807 65

let reminder a b n =
    let rec bitDivisionRec n value1 value2 =
        if bitsToInt n value1 < bitsToInt n value2
        then
            value1
        else
            value2 |> bitDivisionRec n (value1 --& value2) 
    bitDivisionRec n (intToBits a n) (intToBits b n)        
    
let factorA = 16807
let factorB = 48271

let div = intToBits 2147483647 16

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

let (>&) a b =
    if List.length a > List.length b
    then true
    elif List.length a < List.length b
    then false
    else
        List.fold2 (fun s x y -> if x > y then s || true else s && false) true (List.rev a) (List.rev b)

let reminder' a b =
    let rec bitDivisionRec value1 value2 =
        if value1 >& value2
        then
            value2 |> bitDivisionRec (value1 --& value2) 
        else
            value1
            
    bitDivisionRec a b     


reminder' (intToBits 16 8) (intToBits 2 8)

(intToBits 16 8) >& (intToBits 2 8)
(intToBits 16 8) --& (intToBits 2 8)
[0; 0; 0; 0; 1; 1; 1; 0] >& (intToBits 2 8)
[0; 0; 0; 0; 1; 1; 1; 0] --& (intToBits 2 8)

let x = reminder' (bitMultiply 32 65 factorA ) div

reminder (bitMultiply 32 1092455 factorA |> bitsToInt 32) div 32 |> bitsToInt 32

reminder (bitMultiply 32 1181022001 factorA |> bitsToInt 32) div 32

// bitMultiply 16 8921 factorB

// bitMultiply 16 1092455 factorA
// bitMultiply 16 8921 factorB

// intToBits 2147483647 32

checkSum 0 65 8921 5 0