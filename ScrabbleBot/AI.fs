namespace Zyzzyva

open ScrabbleUtil
open Dictionary

module AI =
    let testBool = true
    let testInt = 0
    
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list
    let nextCoord ((x, y): coord) (prefixSearch: bool) isHorizontal =
        if prefixSearch then
            if isHorizontal
            then (x - 1, y)
            else (x, y - 1)
        else
            if isHorizontal
            then (x + 1, y)
            else (x, y + 1)
    let extract = fun (id, (c, p)) -> c
    (*
     Recursive function that attempts to build a word, tile by tile.
     First step with a char and node and checks if this completes a word
     If it does -> return acc word
     If not:
     If we get a sub-node from the step try again with a char from hand
     If we do not get a sub-node (None) then call reverse
     if reverse = Move -> move
     elif reverse = None -> None 
    *)
    let rec buildWord  (tileId:uint32) (node:Dict) (accMove:Move option) (hand:MultiSet.MultiSet<uint32>) (hasBeenReversed:bool) (st: State.state) : Move option =
        // tileA = { (A, 1) }
        // tileWild { (A, 0), (B, 0) ... (Z, 0) }
        let tile = st.tileLookup.[tileId]
        let buildWordFromTile (tileElement:char*int) =
            let extractedCharacter = fst tileElement
            let check = step extractedCharacter node
            
            let updatedMove = accMove |> Option.get |> List.append [(0,0),(tileId, tileElement)] |> Some // Add coord instead of 0,0
            match check with
            | Some (true, _) -> updatedMove //If the we have found a word, simply return the accumilated word
            | Some (false, nextNode) ->
                    let updatedHand = hand |> MultiSet.removeSingle tileId
                    hand |> MultiSet.toList |> List.tryPick (fun tileId -> buildWord tileId nextNode updatedMove updatedHand hasBeenReversed st )
            | None ->
                    if hasBeenReversed then None
                    else
                        match reverse node with
                        | Some (_, reverseNode) -> // Never completes a word
                            let check = buildWord tileId reverseNode accMove hand true st
                            match check with
                            | Some move -> Some move
                            | None -> None
                        | None -> None
        tile |> Set.toList |> List.tryPick (fun tileElement -> buildWordFromTile tileElement)

    let findMoveFromTile (anchorCoord: coord) (st: State.state) (horizontal: bool) : Move option =
            // Initialize search 
            let startingCharacter =
                extract st.placedTiles.[anchorCoord] 
                
            let startingMove = Some [anchorCoord, st.placedTiles.[anchorCoord]] // Convert coord to move
            let check = step startingCharacter st.dict
            match check with
            | None -> None
            | Some (_, startingDict) -> 
                st.hand |> MultiSet.toList |> List.tryPick (fun tileId ->
                    let updatedHand = st.hand |> MultiSet.removeSingle tileId
                    buildWord tileId startingDict startingMove updatedHand false st )

    let nextMove (st: State.state) : Move =
        let findMoveHorizontal =
            Map.tryPick (fun coord _ -> (findMoveFromTile coord st true)) st.placedTiles

        let findMoveVertical =
            Map.tryPick (fun coord _ -> (findMoveFromTile coord st false)) st.placedTiles

        match findMoveHorizontal with
        | Some move -> move
        | None ->
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
