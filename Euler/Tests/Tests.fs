module Tests

open Xunit
open Euler.Numbers;
open FsUnit;

[<TheoryAttribute>]
[<InlineDataAttribute(10UL, 4UL, 2UL)>]
[<InlineDataAttribute(10UL, 10UL, 10UL)>]
let ``Greatest common divisor`` (a, b, expected) =
    greatestCommonDivisor a b |> should equal expected;

[<TheoryAttribute>]
[<InlineDataAttribute(10UL, 4UL, 20UL)>]
[<InlineDataAttribute(3UL, 2UL, 6UL)>]
let ``Least common multiple`` (a, b, expected) =
    leastCommonMultiple a b |> should equal expected;