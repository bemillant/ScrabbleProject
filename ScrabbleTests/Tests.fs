module Tests

open System
open MultiSet
open Xunit
open Zyzzyva
open Dictionary
open ScrabbleUtil
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

let helloGaddag = empty () |> insert "HELLO"

[<Fact>]
let ``gaddag containing HELLO can look up HELLO`` () =
    let canFindHello = helloGaddag |> lookup "HELLO"
    Assert.True canFindHello
    
let tileLookupTable =
    let ids = [1u..26u]
    let letters = ['A' .. 'Z'] |> List.ofSeq
    let tuples = letters |> List.map (fun letter -> (letter, 1))
    let setTuples = tuples |> List.map (fun tuple -> Set.singleton tuple)
    let idsWithSetTuples = List.zip ids setTuples
    idsWithSetTuples |> Map.ofList


[<Fact>]
let ``Id 1u gives tile A`` () =
    let tileA = tileLookupTable.[1u]
    let A = tileA |> Set.maxElement |> fst
    Assert.True ((A = 'A'))
   
// T:20 E:5 S:19 T:20
// add id amount 
let handContainingTest = MultiSet.empty |> add 20u 2u |> add 5u 1u |> add 19u 1u

let dictAPI = Some(Dictionary.empty, Dictionary.insert, Dictionary.step, Some Dictionary.reverse)
let words = seq { "HELLO"; "TEST" }
let dictionary = ScrabbleUtil.Dictionary.mkDict words dictAPI false


[<Fact>]
let ``Build Word`` () =
    AI.buildWord 8u helloGaddag (Some []) handContainingTest false tileLookupTable