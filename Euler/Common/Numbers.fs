namespace Euler

open System.Collections.Generic

module Numbers =

    let greatestCommonDivisor (a: uint64) (b: uint64) =
        let rec gcd dividend divisor =
            let remainder = dividend % divisor
            if remainder = 0UL then
                divisor
            else
                gcd divisor remainder

        gcd (max a b) (min a b)

    let leastCommonMultiple (a: uint64) (b: uint64) =
        let gcd = greatestCommonDivisor (max a b) (min a b)
        (a * b) / gcd

    let toDigits number =
        let stack = new Stack<int>()

        let rec fillStack n = 
            if n < 10 then
                stack.Push n
            else
                let quotient = n / 10
                let remainder = n % 10
                stack.Push remainder
                fillStack quotient

        fillStack number
        seq {
            while stack.Count > 0 do
                yield stack.Pop()
        }

    let rec toReversedDigits number =
        seq {
            if number < 10 then
                yield number
            else
                let quotient = number / 10
                let remainder = number % 10
                yield remainder
                yield! toReversedDigits quotient
        }