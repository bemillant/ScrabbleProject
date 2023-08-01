namespace Zyzzyva

open ScrabbleUtil
open Dictionary

module AI =
    let testBool = true
    let testInt = 0
    
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list

    let nextMove (st: State.state) : Move =
        let findMoveFromTile (anchorCoord: coord) (st: State.state) (horizontal: bool) : Move option =
            let nextCoord ((x, y): coord) (prefixSearch: bool) =
                if prefixSearch then
                    if horizontal
                    then (x - 1, y)
                    else (x, y - 1)
                else
                    if horizontal
                    then (x + 1, y)
                    else (x, y + 1)

            //Recursive function that attempts to build a word, tile by tile.
            //First step with a char and node and checks if this completes a word
            //If it does -> return acc word
            //If not:
            //If we get a sub-node from the step try again with a char from hand
            //If we do not get a sub-node (None) then call reverse
            //if reverse = Move -> move
            //elif reverse = None -> None
            let rec buildWord (tileId:uint32) (node:Dict) (accMove:Move option) (hand:MultiSet.MultiSet<uint32>) (hasBeenReversed:bool) : Move option =
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
                            hand |> MultiSet.toList |> List.tryPick (fun tileId -> buildWord tileId nextNode updatedMove updatedHand hasBeenReversed)
                    | None ->
                            if hasBeenReversed then None
                            else
                                match reverse node with
                                | Some (_, reverseNode) -> // Never completes a word
                                    let check = buildWord tileId reverseNode accMove hand true
                                    match check with
                                    | Some move -> Some move
                                    | None -> None
                                | None -> None
                tile |> Set.toList |> List.tryPick (fun tileElement -> buildWordFromTile tileElement)
            
            // Initialize search 
            let startingCharacter =
                let extract = fun (id, (c, p)) -> c
                extract st.placedTiles.[anchorCoord] 
            let startingMove = Some [anchorCoord, st.placedTiles.[anchorCoord]] // Convert coord to move
            let check = step startingCharacter st.dict
            match check with
            | None -> None
            | Some (_, startingDict) -> 
                st.hand |> MultiSet.toList |> List.tryPick (fun tileId ->
                    let updatedHand = st.hand |> MultiSet.removeSingle tileId
                    buildWord tileId startingDict startingMove updatedHand false)
            // buildWord startingTileId st.dict startingMove st.hand false
            //Recursive method to call after having called reverse
            //Maybe jump coord back to start?
            //Call step on (anchor?) node
            //If word is found -> Move
            //else
            //If we get a sub-node, call step again with a char
            //if we get None -> none

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