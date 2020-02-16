namespace Euler

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