module Tests

open System
open Xunit
open Zyzzyva

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``true`` () =
    let b = AI.testBool
    Assert.True(b)

[<Fact>]
let ``zero`` () =
    let z = AI.testInt
    Assert.True((z = 0))
    