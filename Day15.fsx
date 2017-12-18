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
        let bitSum x y trans =
            match x, y, trans with
            | 0, 0, 0 -> 0, 0
            | 0, 0, 1 -> 1, 0
            | 1, 1, 0 -> 0, 1
            | 1, 1, 1 -> 1, 1
            | _, _, 0 -> 1, 0
            | _, _, 1 -> 0, 1

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

//binary multipy
let bitMultiply n value1 value2 =
    let b1 = intToBits value1 n
    let b2 = intToBits value2 n |> List.rev
    b2 |> List.mapi (fun i x -> if x = 1 then b1 <<<& i else []) |> List.fold ( ++& ) []

bitMultiply 16 16807 65

let rec reminder a b =
    let shift = List.length a - List.length b
    if shift < 0 
    then a 
    else 
        let a' = List.rev a
        let b' = List.rev b <<< & shift
        List.map2 (fun x y -> )
// let factorA = 16807
// let factorB = 48271

// let rec checkSum n prevA prevB count sum =
//     if n < count
//     then 
//         let currA = bitMultiply 16 prevA factorA |> bitsToInt 16
//         let currB = bitMultiply 16 prevB factorB |> bitsToInt 16
//         printfn "%d %d" currA currB
//         let sum = if currA = currB then sum + 1 else sum
//         checkSum (n + 1) currA currB count sum 
//     else
//         sum    


// bitMultiply 16 65 factorA
// bitMultiply 16 8921 factorB

// bitMultiply 16 1092455 factorA
// bitMultiply 16 8921 factorB

// intToBits 2147483647 32

// checkSum 0 65 8921 5 0