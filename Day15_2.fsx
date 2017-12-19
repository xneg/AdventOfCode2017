let rec checkSum n prevA prevB count sum =
    let factorA = 16807I
    let factorB = 48271I
    let div = 2147483647I

    let get16bit (value : bigint) = value.ToByteArray() |> Array.truncate 2
    
    if n < count
    then 
        let currA = (prevA * factorA) % div
        let currB = (prevB * factorB) % div

        let sum = if get16bit currA = get16bit currB then sum + 1 else sum
        checkSum (n + 1) currA currB count sum 
    else
        sum

checkSum 0 703I 516I 40000000 0

let rec checkSum' n prevA prevB count sum =
    let factorA = 16807I
    let factorB = 48271I
    let div = 2147483647I

    let rec generate factor prev mult =
        let curr = (prev * factor) % div
        if curr % mult = 0I then curr else generate factor curr mult

    let get16bit (value : bigint) = value.ToByteArray() |> Array.truncate 2
    
    if n < count
    then 
        let currA = generate factorA prevA 4I
        let currB = generate factorB prevB 8I

        let sum = if get16bit currA = get16bit currB then sum + 1 else sum
        checkSum' (n + 1) currA currB count sum 
    else
        sum

checkSum' 0 703I 516I 5000000 0    