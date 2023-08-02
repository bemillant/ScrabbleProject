namespace Zyzzyva

open ScrabbleUtil
open Dictionary

module AI =
    let testBool = true
    let testInt = 0
    
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list
    let nextCoord ((x, y): coord) (prefixSearch: bool) isHorizontal ((anchorX, anchorY): coord) =
        if prefixSearch then
            if isHorizontal
            then (x - 1, y)
            else (x, y - 1)
        else
            if x + y < anchorX + anchorY 
            then if isHorizontal
                 then (anchorX + 1, anchorY)
                 else (anchorX, anchorY + 1)
            else if isHorizontal
                 then (x + 1, y)
                 else (x, y + 1)
    let extract = fun (id, (c, p)) -> c

    let updateAcc (accMove: Move option) coord ((tileId:uint32), (tileElement:char*int)) = 
        accMove |> Option.get |> List.append [coord, (tileId, tileElement)] |> Some

    let rec buildWord  (tileId:uint32) (coord: coord) (node:Dict) (accMove:Move option) (hand:MultiSet.MultiSet<uint32>) (hasBeenReversed:bool) (tileMap: Map<uint32,tile>) (isHorizontal: bool) (anchorCoord: coord): Move option =
        // tileA = { (A, 1) }
        // tileWild { (A, 0), (B, 0) ... (Z, 0) }
        let tile = tileMap.[tileId]
        let buildWordFromTile (tileElement:char*int) =
            let extractedCharacter = fst tileElement
            let check = step extractedCharacter node
            let newCoord = nextCoord coord (not hasBeenReversed) isHorizontal anchorCoord
            let updatedAccMove = updateAcc accMove newCoord (tileId, tileElement)

            //let updatedMove = accMove |> Option.get |> List.append [(0,0),(tileId, tileElement)] |> Some // Add coord instead of 0,0
            match check with
            | Some (true, _) -> updatedAccMove //If the we have found a word, simply return the accumilated word
            | Some (false, nextNode) ->
                    let updatedHand = hand |> MultiSet.removeSingle tileId
                    hand |> MultiSet.toList |> List.tryPick (fun tileId -> buildWord tileId newCoord nextNode updatedAccMove updatedHand hasBeenReversed tileMap isHorizontal anchorCoord )
            | None ->
                    if hasBeenReversed then None
                    else
                        match reverse node with
                        | Some (_, reverseNode) -> // Never completes a word
                            let check = buildWord tileId newCoord reverseNode accMove hand true tileMap isHorizontal anchorCoord
                            match check with
                            | Some move -> Some move
                            | None -> None
                        | None -> None
        tile |> Set.toList |> List.tryPick (fun tileElement -> buildWordFromTile tileElement)

    let findMoveFromTile (anchorCoord: coord) (st: State.state) (horizontal: bool) : Move option =
            // Initialize search 
            let startingCharacter =
                extract st.placedTiles.[anchorCoord] 
                
            let startingMove = Some [anchorCoord, st.placedTiles.[anchorCoord]] // Update to be empty!
            let check = step startingCharacter st.dict
            match check with
            | None -> None
            | Some (_, startingDict) -> 
                st.hand |> MultiSet.toList |> List.tryPick (fun tileId ->
                    let updatedHand = st.hand |> MultiSet.removeSingle tileId
                    buildWord tileId anchorCoord startingDict startingMove updatedHand false st.tileLookup horizontal anchorCoord )

    let findWordOnEmptyBoard (st: State.state)=
        //If the board is empty, then we want to pick a word from our hand
            st.hand 
            |> MultiSet.toList 
            |> List.tryPick (fun charId -> 
                                                let tile = st.tileLookup.[charId]
                                                tile 
                                                |> Set.toList 
                                                |> List.tryPick (fun tileElement -> 
                                                                                                        let initStep = step (fst tileElement) st.dict 
                                                                                                        let startingMove = updateAcc (Some []) (0,0) (charId, tileElement)
                                                                                                        let updatedHand = st.hand |> MultiSet.removeSingle charId
                                                                                                        match initStep with
                                                                                                        |Some (_,d) -> buildWord charId (0,0) d (startingMove) updatedHand false st.tileLookup true (0,0)
                                                                                                        |None-> None
                                                )
                                    )


    (*First try to find moves horizontally then if no move was found, try finding a word vertically*)
    let nextMove (st: State.state) : Move =
        printfn "%A" st.placedTiles
        if st.placedTiles.IsEmpty then
            match findWordOnEmptyBoard st with
            |Some move -> move
            |None -> failwith "Did not find a starting word!"



        else
            let findMoveHorizontal =
                Map.tryPick (fun coord _ -> (findMoveFromTile coord st true)) st.placedTiles

            match findMoveHorizontal with
            | Some move -> move
            | None ->
                let findMoveVertical = Map.tryPick (fun coord _ -> (findMoveFromTile coord st false)) st.placedTiles
                match findMoveVertical with
                | Some move -> move
                | None -> failwith "not implemented" // swap out tiles



// iterate over all tiles on the board (using tryPick)
// go through the dictionary starting from the tile
// test if a node containing "isWord" can be reached given
// the tiles on the board and on the hand
// test both vertically and horizontally
// if a word is found using only tiles on the board
// (non from the hand) then the move is not valid
// return the first valid move found
// if no valid move is found, give up all tiles
