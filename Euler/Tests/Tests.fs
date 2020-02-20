module Tests

open Xunit
open Euler.Numbers;
open FsUnit;

[<Theory>]
[<InlineData(10UL, 4UL, 2UL)>]
[<InlineData(10UL, 10UL, 10UL)>]
let ``Greatest common divisor`` (a, b, expected) =
    greatestCommonDivisor a b |> should equal expected

[<Theory>]
[<InlineData(10UL, 4UL, 20UL)>]
[<InlineData(3UL, 2UL, 6UL)>]
let ``Least common multiple`` (a, b, expected) =
    leastCommonMultiple a b |> should equal expected

let sequenceEqual seq1 seq2 =
    set seq1 = set seq2

[<Fact>]
let ``Some numbers to digits`` () =
    sequenceEqual (toDigits 120) [ 1; 2; 0 ] |> should equal true
    sequenceEqual (toDigits 4) [ 4 ] |> should equal true

[<Fact>]
let ``Some numbers to reversed digits`` () =
    sequenceEqual (toReversedDigits 120) [ 0; 2; 1 ] |> should equal true
    sequenceEqual (toReversedDigits 4) [ 4 ] |> should equal true