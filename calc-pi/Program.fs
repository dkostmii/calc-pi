let modularPow b exp modulus =
    if (modulus = 1) then
        0
    else
        let folder = fun c b -> c * b % modulus
        Seq.fold folder 1 (List.replicate exp b)


let rec positiveModulus (a : float) (n : float) =
    if a < 0 then
       positiveModulus (a + n) n
    else
        a % n

let S j n =
    let kRange = [0..1..n]
    let leftSumFolder (sumSoFar: float) k =
        let r = 8 * k + j
        (sumSoFar + ((modularPow 16 (n - k) r) |> float) / (r |> float)) % 1.0

    let leftSum = Seq.fold leftSumFolder 0.0 kRange

    let rec rightSumStep tSoFar kSoFar =
        let t = tSoFar + (pown 16.0 (n - kSoFar)) / ((8 * kSoFar + j) |> float)
        if t = tSoFar then t
        else rightSumStep t (kSoFar + 1)

    let rightSum = rightSumStep 0.0 (n + 1)

    leftSum + rightSum

let piDigit n =
    let nZeroBased = n - 1
    let x = (4.0 * (S 1 nZeroBased) - 2.0 * (S 4 nZeroBased) - (S 5 nZeroBased) - (S 6 nZeroBased))
    let xMod = positiveModulus x 1.0
    let xShift = pown 16.0 14
    let xShifted = xMod * xShift

    sprintf "%014x" (xShifted |> int64)

            
printfn "%s" (piDigit 0)