module Tests

open Xunit
open Euler.Numbers;
open FsUnit;

[<Theory>]
[<InlineData(10UL, 4UL, 2UL)>]
[<InlineData(10UL, 10UL, 10UL)>]
let ``Greatest common divisor`` (a, b, expected) =
    greatestCommonDivisor a b |> should equal expected;

[<Theory>]
[<InlineData(10UL, 4UL, 20UL)>]
[<InlineData(3UL, 2UL, 6UL)>]
let ``Least common multiple`` (a, b, expected) =
    leastCommonMultiple a b |> should equal expected;

[<Fact>]
let ``Some numbers to digits`` () =
    toDigits 120 |> should equivalent [ 1; 2; 0 ]
    toDigits 4 |> should equivalent [ 4 ]