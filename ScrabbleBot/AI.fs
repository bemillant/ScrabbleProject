namespace Zyzzyva

open ScrabbleUtil
open Dictionary

module AI =
    let testBool = true
    let testInt = 0
    
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list
    let getNextCoord ((x, y): coord) ((anchorX, anchorY): coord) (prefixSearch: bool) (isHorizontal: bool) =
        if prefixSearch then
            if isHorizontal
            then (x - 1, y)
            else (x, y - 1)
        else
            if x + y < anchorX + anchorY  // We feed anchorcoord, so this should never happen!
            then if isHorizontal
                 then (anchorX + 1, anchorY)
                 else (anchorX, anchorY + 1)
            else if isHorizontal
                 then (x + 1, y)
                 else (x, y + 1)
    let extract = fun (id, (c, p)) -> c

    let updateAcc (accMove: Move option) coord ((tileId:uint32), (tileElement:char*int)) = 
        accMove |> Option.get |> List.append [coord, (tileId, tileElement)] |> Some

    let rec next
        (lastCoord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) // changes through recursion
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) // stays the same
        : Move option = // return type
            
        let nextCoord = getNextCoord lastCoord anchorCoord isPrefixSearch isHorizontal
        let boardTile : (uint32*(char*int)) option = placedTiles.TryFind nextCoord
            
        match boardTile with
        | Some (id, (c, p)) -> tryBoardTile c nextCoord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles
        | None -> tryHand nextCoord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles
    and tryBoardTile
        (character:char)
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option)
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>)
        : Move option =
            
        let result = step character node
        match result with
        | Some (true, _) -> accMove
        | Some (false, node) -> next coord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles
        | None -> None
        
    and tryHand
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option)
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>)
        : Move option =
        
        let updatedAccMove (accMove: Move option) (coord:coord) (tileId:uint32) (c:char) (p:int) = 
            accMove |> Option.get |> List.append [coord, (tileId, (c, p))] |> Some

        let tryLetter (id, (c, p)) =
            let result = step c node
            match result with
            | Some (true, _) ->
                updatedAccMove accMove coord id c p
            | Some (false, node) ->
                let updatedMove = updatedAccMove accMove coord id c p
                next coord node hand isPrefixSearch updatedMove anchorCoord isHorizontal idTileLookup placedTiles
            | None ->
                if isPrefixSearch 
                then
                    let result = reverse node
                    match result with
                    | Some (_, reverseNode) ->
                        next anchorCoord reverseNode hand false accMove anchorCoord isHorizontal idTileLookup placedTiles
                    | None -> None
                else None
            
        let tryTileId id = idTileLookup.[id] |> Set.toList |> List.tryPick (fun (c, p) -> tryLetter (id, (c, p)))
        hand |> MultiSet.toList |> List.tryPick (fun id -> tryTileId id)
    
    let initializeSearch (anchorCoord: coord) (st: State.state) (isHorizontal: bool) : Move option =
        // Initialize search 
        let startingMove = Some []
        let startingCharacter = extract st.placedTiles.[anchorCoord] 
        let result = step startingCharacter st.dict
        match result with
        | Some (_, startingDict) -> next anchorCoord startingDict st.hand true startingMove anchorCoord isHorizontal st.tileLookup st.placedTiles
        | None -> None

    let findWordOnEmptyBoard (st: State.state)=
        next (0,0) st.dict st.hand true (Some []) (0,0) true st.tileLookup st.placedTiles

    (*First try to find moves horizontally then if no move was found, try finding a word vertically*)
    let nextMove (st: State.state) : Move =
        if st.placedTiles.IsEmpty then
            match findWordOnEmptyBoard st with
            | Some move -> move
            | None -> failwith "Did not find a starting word!"
        else
            let findMoveHorizontal = Map.tryPick (fun coord _ -> (initializeSearch coord st true)) st.placedTiles
            match findMoveHorizontal with
            | Some move -> move
            | None ->
                let findMoveVertical = Map.tryPick (fun coord _ -> (initializeSearch coord st false)) st.placedTiles
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
