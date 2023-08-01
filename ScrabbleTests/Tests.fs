module Tests

open System
open Xunit
open Zyzzyva
open Dictionary
// open MultiSet

// [<Fact>]
// let ``My test`` () =
//     Assert.True(true)
//
// [<Fact>]
// let ``true`` () =
//     let b = AI.testBool
//     Assert.True(b)
//
// [<Fact>]
// let ``zero`` () =
//     let z = AI.testInt
//     Assert.True((z = 0))
    
// [<Fact>]
// let ``gaddag of A can step from root to A`` () =
//     let gaddag = empty () |> insert "A"
//     let result = gaddag |> step 'A'
//     let isEndOfWord =
//         match result with
//         | Some (true, _) -> true
//         | Some (false, _) -> false // this is the outcome ???
//         | None -> false
//     Assert.True(isEndOfWord)
    
[<Fact>]
let ``gaddag of A can find word A`` () =
    let gaddag = empty () |> insert "A"
    let canFindA = gaddag |> lookup "A"
    Assert.True canFindA
    
    
[<Fact>]
let ``gaddag of A has node count of 2`` () =
    let gaddag = empty () |> insert "A"
    let nodeCount = gaddag |> Dictionary.nodeCount
    Assert.True((nodeCount = 2))
        
// let ``hand of A`` () =
    // let handOfA = empty |> addSingle 1u
    // let tileMap = Map.empty |> Map.add 1u Set ('A', 1)
    
[<Fact>]
let ``Board containing A has 1 tile`` () =
    let centerCoord = (0,0)
    let tileA = (1u, ('A', 1))
    let placedTiles = Map.empty |> Map.add centerCoord tileA
    Assert.True (placedTiles.Count = 1)
    
let fiveLetterWordCoordinates = [(0,0); (1,0); (2,0); (3,0); (4,0)]
let tileH = (8u, ('H', 1))
let tileE = (5u, ('E', 1))
let tileL = (12u, ('L', 1))
let tileO = (15u, ('O', 1))
let helloTiles = [tileH; tileE; tileL; tileL; tileO]

let helloOnBoard = List.zip fiveLetterWordCoordinates helloTiles |> Map.ofList // map from coord to tile

[<Fact>]
let ``Board containing HELLO has 5 tiles`` () =
    Assert.True (helloOnBoard.Count = 5)
    