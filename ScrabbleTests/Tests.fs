module Tests

open System
open MultiSet
open Xunit
open Zyzzyva
open Dictionary
open ScrabbleUtil
open Zyzzyva.AI
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
    
[<Fact>]
let ``gaddag of A can step from root to A`` () =
    let gaddag = empty () |> insert "A"
    let result = gaddag |> step 'A'
    let isEndOfWord =
        match result with
        | Some (true, _) -> true
        | Some (false, node) ->
            let result = reverse node
            match result with
            | Some (true, _) -> false
            | Some (false, _) -> false
            | None -> false
            // false // this is the outcome ???
        | None -> false
    Assert.True(isEndOfWord)


[<Fact>]
let ``gaddag of AB can step from root to BA#`` () =
    let gaddag = empty () |> insert "AB"
    let result = gaddag |> step 'B'
    let isEndOfWord =
        match result with
        | Some (true, _) -> false
        | Some (false, node) ->
            let result = step 'A' node
            match result with
            | Some (true, _) -> false
            | Some (false, node) ->
                let result = reverse node
                match result with
                | Some _ -> true
                | None -> false
            | None -> false
            // false // this is the outcome ???
        | None -> false
    Assert.False(isEndOfWord)
    
[<Fact>]
let ``gaddag of AB can step from root to BA`` () =
    let gaddag = empty () |> insert "AB"
    let result = gaddag |> step 'B'
    let isEndOfWord =
        match result with
        | Some (true, _) -> false
        | Some (false, node) ->
            let result = step 'A' node
            match result with
            | Some (true, _) -> true
            | Some (false, node) ->
                let result = reverse node
                match result with
                | Some _ -> false
                | None -> false
            | None -> false
            // false // this is the outcome ???
        | None -> false
    Assert.True(isEndOfWord)

[<Fact>]
let ``gaddag of AB can step from root to A#B`` () =
    let gaddag = empty () |> insert "AB"
    let result = gaddag |> step 'A'
    let isEndOfWord =
        match result with
        | Some (true, _) -> false
        | Some (false, node) ->
            let result = reverse node
            match result with
            | Some (true, _) -> false
            | Some (false, node) ->
                let result = step 'B' node
                match result with
                | Some _ -> true
                | None -> false
            | None -> false
            // false // this is the outcome ???
        | None -> false
    Assert.True(isEndOfWord)

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

let wildcardTile =
    let letters = ['A' .. 'Z'] |> List.ofSeq
    let tuples = letters |> List.map (fun letter -> (letter, 0))
    let set = Set.ofList tuples
    (0u, set)
    
let tileLookupTable =
    let ids = [1u..26u]
    let letters = ['A' .. 'Z'] |> List.ofSeq
    let tuples = letters |> List.map (fun letter -> (letter, 1))
    let setTuples = tuples |> List.map (fun tuple -> Set.singleton tuple)
    let idsWithSetTuples = List.zip ids setTuples
    let tiles = idsWithSetTuples @ [wildcardTile]    
    tiles |> Map.ofList

let idLookupTable =
    let letters = ['A' .. 'Z'] |> List.ofSeq
    let ids = [1u..26u]
    let combined = List.zip letters ids |> List.append ['*', 0u]
    Map.ofList combined

[<Fact>]
let ``Id 1u gives tile A`` () =
    let tileA = tileLookupTable.[1u]
    let A = tileA |> Set.maxElement |> fst
    Assert.True ((A = 'A'))
   
// T:20 E:5 S:19 T:20
// add id amount 
let handContainingTest = MultiSet.empty |> add idLookupTable.['T'] 2u |> addSingle idLookupTable.['E'] |> addSingle idLookupTable.['S']

let words_HELLO_TEST = seq { "HELLO"; "TEST" }
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)
    
let dictAPI = Some(empty, insert, step, Some reverse)
let (dictionary_HELLO_TEST, _) =
    time (fun () -> ScrabbleUtil.Dictionary.mkDict words_HELLO_TEST dictAPI)
let _HELLO_TEST_dict = dictionary_HELLO_TEST true


let words_TEST = seq { "TEST" }
let (dictionary_TEST, _) =
    time (fun () -> ScrabbleUtil.Dictionary.mkDict words_TEST dictAPI)
let _TEST_dict = dictionary_TEST true

[<Fact>]
let ``_HELLO_TEST_dict contains word HELLO`` () =
    let containsHello = _HELLO_TEST_dict |> Dictionary.lookup "HELLO"
    Assert.True containsHello

[<Fact>]
let ``_HELLO_TEST_dict contains word TEST`` () =
    let containsHello = _HELLO_TEST_dict |> Dictionary.lookup "TEST"
    Assert.True containsHello


[<Fact>]
let ``TEST_dict contains word TEST`` () =
    let containsTest = _TEST_dict |> Dictionary.lookup "TEST"
    Assert.True containsTest

let coord00 = (0,0)
let coord10 = (1,0)

let placedTilesE = Map.empty |> Map.add (0,0) (idLookupTable.['E'], ('E', 1))
let handContainingTst = MultiSet.empty |> add idLookupTable.['T'] 2u |> addSingle idLookupTable.['S']


[<Fact>]
let ``Build Word TEST from an E given hand TEST and dictionary TEST`` () =
    let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesE
    let foundWord =
        match move with
        | Some _ -> true
        | None -> false
    Assert.True foundWord
    
let expectedTestCoordinates = [(-1,0); (1,0); (2,0)]
let extractCoords (move:Move) = move |> List.map (fun (coord, (id, (c, p))) -> coord)
let extractLetters (move:Move) = move |> List.map (fun (coord, (id, (c, p))) -> c)

[<Fact>]
let ``Move TEST from an E places 3 tiles`` () =
    let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesE
    let amountOfCoordinates =
        match move with
        | Some move -> (extractCoords move).Length
        | None -> 0
    Assert.Equal (3, amountOfCoordinates)

[<Fact>]
let ``Move TEST from an E places tiles on (-1,0) (1,0) (2,0)`` () =
    let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesE
    let actualCoordinates : (int*int) list =
        match move with
        | Some move -> extractCoords move
        | None -> []
    let equal = List.sort expectedTestCoordinates = List.sort actualCoordinates
    Assert.True equal
    
let placedTilesET = Map.empty |> Map.add (0,0) (idLookupTable.['E'], ('E', 1)) |> Map.add (2,0) (idLookupTable.['T'], ('T', 1))

[<Fact>]
let ``Move TEST from an E and an T to the right can write TEST`` () =
    let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesET
    let foundWord =
        match move with
        | Some _ -> true
        | None -> false
    Assert.True foundWord
    
let placedTilesEX = Map.empty |> Map.add (0,0) (idLookupTable.['E'], ('E', 1)) |> Map.add (2,0) (idLookupTable.['X'], ('X', 1))

[<Fact>]
let ``Move TEST from an E and an X to the right can not write TEST`` () =
    let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesEX
    let foundWord =
        match move with
        | Some _ -> true
        | None -> false
    Assert.False foundWord
    
let placedTilesE_WithXBlocking = Map.empty |> Map.add (0,0) (idLookupTable.['E'], ('E', 1)) |> Map.add (3,0) (idLookupTable.['X'], ('X', 1))
[<Fact>]
let ``Move TEST from an E and an X blocking to the right at (3,0) can not write TEST`` () =
    let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesE_WithXBlocking
    let foundWord =
        match move with
        | Some _ -> true
        | None -> false
    Assert.False foundWord
    
// [<Fact>]
// let ``Build Word TEST from T given hand TEST and dictionary TEST`` () =
//     let move = AI.buildWord idLookupTable.['T'] coord00 _TEST_dict (Some []) handContainingTest false tileLookupTable false coord00
//     let foundWord =
//         match move with
//         | Some word -> true
//         | None -> false
//     Assert.True foundWord
//     
// [<Fact>]
// let ``Build Word TEST from S given hand TEST and dictionary TEST`` () =
//     let move = AI.buildWord idLookupTable.['S'] coord00 _TEST_dict (Some []) handContainingTest false tileLookupTable false coord00
//     let foundWord =
//         match move with
//         | Some word -> true
//         | None -> false
//     Assert.True foundWord
//     
// [<Fact>]
// let ``Build Word TEST from W given hand TEST and dictionary TEST should not be possible`` () =
//     let move = AI.buildWord idLookupTable.['W'] coord00 _TEST_dict (Some []) handContainingTest false tileLookupTable false coord00
//     let foundWord =
//         match move with
//         | Some word -> true
//         | None -> false
//     Assert.False foundWord
//     
// [<Fact>]
// let ``Build Word TEST from E given hand TEST and dictionary TEST_HELLO`` () =
//     let move = AI.buildWord idLookupTable.['E'] coord00 _HELLO_TEST_dict (Some []) handContainingTest false tileLookupTable false coord00
//     let foundWord =
//         match move with
//         | Some word -> true
//         | None -> false
//     Assert.True foundWord
//
// let wildcardHand = MultiSet.empty |> add idLookupTable.['*'] 4u
//
// [<Fact>] // * = wildcard
// let ``Build Word TEST from E given hand **** and dictionary TEST_HELLO`` () =
//     let move = AI.buildWord idLookupTable.['E'] coord00 _HELLO_TEST_dict (Some []) wildcardHand false tileLookupTable false coord00
//     let foundWord =
//         match move with
//         | Some word -> true
//         | None -> false
//     Assert.True foundWord
//
//
// [<Fact>]
// let ``Build Word TEST from E given hand TST and dictionary TEST_HELLO`` () =
//     let move = AI.buildWord idLookupTable.['E'] coord00 _HELLO_TEST_dict (Some []) handContainingTst false tileLookupTable false coord00
//     let foundWordSequence =
//         match move with
//         | Some word -> true
//         | None -> false
//
//     Assert.True (foundWordSequence)
//     
//     
// let extractCoords (move:Move) = move |> List.map (fun (coord, (id, (c, p))) -> coord)
// let extractLetters (move:Move) = move |> List.map (fun (coord, (id, (c, p))) -> c)
//     
// let coords_00_02_30 = [(0,0); (1,0); (2,0); (3,0)]
//
//
// [<Fact>]
// let ``Build Word TEST from a E on (0,0) given hand TEST and dictionary TEST puts tiles on coordinates (0,0) - (3,0)`` () =
//     let move = next coord00 _TEST_dict handContainingTst true (Some []) coord00 true tileLookupTable placedTilesE
//     // let move = AI.buildWord idLookupTable.['E'] coord10 _TEST_dict (Some []) handContainingTest false tileLookupTable true coord10
//     let coords : (int*int) list =
//         match move with
//         | Some move -> extractCoords move
//         | None -> []
//     Assert.Equivalent ((List.sort coords_00_02_30), (List.sort coords))
//     
// [<Fact>]
// let ``Build Word TEST from a E on (1,0) given hand TEST and dictionary TEST places 3 tiles`` () =
//     let move = AI.buildWord idLookupTable.['E'] coord10 _TEST_dict (Some []) handContainingTest false tileLookupTable true coord10
//     let tilesPlaced =
//         match move with
//         | Some move -> move.Length
//         | None -> 0
//     Assert.Equal (3, tilesPlaced)
//     
//     
// [<Fact>]
// let ``Build Word TEST from a E given hand TEST and dictionary TEST puts tiles (T S T)`` () =
//     let move = AI.buildWord idLookupTable.['E'] coord10 _TEST_dict (Some []) handContainingTest false tileLookupTable true coord10
//     let letters =
//         match move with
//         | Some move -> extractLetters move
//         | None -> []
//     Assert.Equivalent ((List.sort ['T'; 'S'; 'T']), (List.sort letters))