open System


// NORMAL RECURSION


// FACTORIAL DE N (1 * 2 * 3 ... N)
let rec rFACTORIAL (num : double) : double = 
    if num <= double(1) then
        double(1)
    else
        rFACTORIAL(num - double(1)) * double(num)

// SUMATORIA DE LOS NUMEROS DE 1 A N : SUM(N)
let rec recSum (num : double) : double =
    if num <= double(1) then
        double(num)
    else
        recSum(num - double(1)) + double(num)


// TAIL RECURSION


// FACTORIAL DE N (1 * 2 * 3 ... N)
let tFACTORIAL num : double =
    let rec factorial num acc : double =
        if num <= double(1) then  
            acc
        else
            factorial (num - double(1)) (acc * num)
    factorial num (double(1))

// SUMATORIA DE LOS NUMEROS DE 1 A N : SUM(N)
let tsum (num : double) : double =
    let rec sum (num : double) (acc : double) : double =
        if num <= double(1) then  
            double(acc)
        else
            sum (num - double(1)) (acc + num)
    sum (double(num)) (double(1))

// MAXIMO COMUN DIVISOR (EUCLID'S ALGORITHM)
let rec tEUCLIDSALG (num1 : double) (num2 : double) : double =
    if num2 <= double(0) then
        num1
    else
        tEUCLIDSALG (double(num2)) (num1 % num2)


// ITERANDO


// FACTORIAL DE N (1 * 2 * 3 ... N) ITERANDO
let iFACTORIAL (num : double) : double =
    let mutable fact : double = double(1)
    if num < double(2) then
        double(num)
    else
        for i in num .. double(-1) .. double(1) do
            fact <- fact * (double(i))
        double(fact)

// SUMATORIA DE LOS NUMEROS DE 1 A N : SUM(N) ITERANDO
let iSUM (num : double) : double =
    let mutable sum : double = double(0)
    if num < double(2) then
        double(num)
    else
        for i in num .. double(-1) .. double(0) do
            sum <- sum + double(i)
        double(sum)

// MAXIMO COMUN DIVISOR (EUCLID'S ALGORITHM)
let iEUCLIDSALG n1 n2 : double =
    let mutable num1 = n1 : double
    let mutable num2 = n2 : double
    let mutable modulo = (n1 % n2) : double
    while modulo > double(0) do
        num1 <- num2
        num2 <- modulo
        modulo <- num1 % num2
    double(num2)
[<EntryPoint>]
let main argv = 
    printfn "Factorial!"
    let factorialRECURSIVO : double = rFACTORIAL (double(50))
    printfn "Recursion normal: %f" factorialRECURSIVO
    let factorialTAILRECURSION : double = tFACTORIAL (double(50))
    printfn "Tail Recursion: %f" factorialTAILRECURSION
    let factorialITERATIVO : double = iFACTORIAL (double(50))
    printfn "Iterando: %f" factorialITERATIVO
    printfn ""

    printfn "Sumatoria!"
    let sumRECURSIVO : double = recSum (double(50))
    printfn "Recursion normal: %f" sumRECURSIVO
    let sumTAILRECURSION : double = tsum (double(50))
    printfn "Tail Recursion: %f" sumTAILRECURSION
    let sumITERATIVO : double = iSUM (double(50))
    printfn "Iterando: %f" sumITERATIVO
    printfn ""

    printfn "Maximo comun divisor! (GCD)"
    let euclidsalgTAILRECURSION : double = tEUCLIDSALG (double(48)) (double(60))
    printfn "Tail Recursion: %f" euclidsalgTAILRECURSION
    let euclidsalgITERATIVO : double = iEUCLIDSALG (double(48)) (double(60))
    printfn "Iterando: %f" euclidsalgITERATIVO
    printfn ""
    
    0