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