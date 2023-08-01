module Tests

open System
open Xunit
open Zyzzyva
open MultiSet

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
    
[<Fact>]
let ``gaddag of A can step to A from root`` () =
    let gaddag = Dictionary.empty () |> Dictionary.insert "A"
    let result = gaddag |> Dictionary.step 'A'
    let isEndOfWord =
        match result with
        | Some (true, _) -> true
        | Some (false, _) -> false // this is the outcome ???
        | None -> false
    Assert.True(isEndOfWord)
    
[<Fact>]
let ``gaddag of A has node count of 2`` () =
    let gaddag = Dictionary.empty () |> Dictionary.insert "A"
    let nodeCount = gaddag |> Dictionary.nodeCount
    Assert.True((nodeCount = 2))
        
// let ``hand of A`` () =
    // let handOfA = empty |> addSingle 1u
    // let tileMap = Map.empty |> Map.add 1u Set ('A', 1)
