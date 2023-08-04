namespace Zyzzyva

open ScrabbleUtil
open Dictionary

module AI =
    let testBool = true
    let testInt = 0
    
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list
    let getNextCoord ((x, y): coord) (prefixSearch: bool) (isHorizontal: bool) =
        let a =
            if prefixSearch
            then -1
            else 1
        if isHorizontal
        then (x + a, y)
        else (x, y + a)
        
    let extract = fun (id, (c, p)) -> c

    let updateAcc (accMove: Move option) coord ((tileId:uint32), (tileElement:char*int)) = 
        accMove |> Option.get |> List.append [coord, (tileId, tileElement)] |> Some

    let rec next
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) (hasFoundWord:bool) // changes through recursion
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) (squares:Parser.boardFun2) // stays the same
        : Move option = // return type
            
        let isOutOfBounds =
            match squares coord with
            | StateMonad.Failure error -> true // Will never happen
            | StateMonad.Success square ->
                match square with
                | Some _ -> false
                | None -> true
        
        if isOutOfBounds
        then None
        else
            let boardTile : (uint32*(char*int)) option = placedTiles.TryFind coord
                
            match boardTile with
            | Some (id, (c, p)) -> tryBoardTile c coord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles squares
            | None ->
                if hasFoundWord 
                then
                    if isPrefixSearch
                    then
                        let result = node |> reverse
                        match result with
                        | Some (_, reverseNode) ->
                            let nextCoord = getNextCoord anchorCoord false isHorizontal
                            next nextCoord reverseNode hand false accMove true anchorCoord isHorizontal idTileLookup placedTiles squares
                        | None -> failwith "Not possible" // reverse and call next with that node
                    else
                        let move = accMove |> Option.get
                        if move.IsEmpty then None
                        else accMove // return move
                else tryHand coord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles squares
    and tryBoardTile
        (character:char)
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) 
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) (squares:Parser.boardFun2) 
        : Move option =
            
        let result = step character node
        match result with
        | Some (true, node) ->
            let nextCoord = getNextCoord coord isPrefixSearch isHorizontal
            next nextCoord node hand isPrefixSearch accMove true anchorCoord isHorizontal idTileLookup placedTiles squares
        | Some (false, node) ->
            let nextCoord = getNextCoord coord isPrefixSearch isHorizontal
            next nextCoord node hand isPrefixSearch accMove false anchorCoord isHorizontal idTileLookup placedTiles squares
        | None -> None
        
    and tryHand
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) 
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) (squares:Parser.boardFun2) 
        : Move option =
        
        let updatedAccMove (accMove: Move option) (coord:coord) (tileId:uint32) (c:char) (p:int) = 
            accMove |> Option.get |> List.append [coord, (tileId, (c, p))] |> Some

        let tryLetter (id, (c, p)) =
            let result = step c node
            match result with
            | Some (foundWord, node) ->
                let nextCoord = getNextCoord coord isPrefixSearch isHorizontal
                let updatedMove = updatedAccMove accMove coord id c p
                let updatedHand = hand |> MultiSet.removeSingle id
                next nextCoord node updatedHand isPrefixSearch updatedMove foundWord anchorCoord isHorizontal idTileLookup placedTiles squares
            | None ->
                if isPrefixSearch 
                then
                    let result = reverse node
                    match result with
                    | Some (_, reverseNode) ->
                        let nextCoord = getNextCoord anchorCoord false isHorizontal
                        next nextCoord reverseNode hand false accMove false anchorCoord isHorizontal idTileLookup placedTiles squares
                    | None -> None
                else None
            
        let tryTileId id = idTileLookup.[id] |> Set.toList |> List.tryPick (fun (c, p) -> tryLetter (id, (c, p)))
        hand |> MultiSet.toList |> List.tryPick (fun id -> tryTileId id)
    
    let initializeSearch (anchorCoord: coord) (st: State.state) (isHorizontal: bool) : Move option =
        // Initialize search 
        let startingMove = Some []
        let startingCharacter = extract st.placedTiles.[anchorCoord] 
        let result = step startingCharacter st.dict
        next anchorCoord st.dict st.hand true startingMove false anchorCoord isHorizontal st.tileLookup st.placedTiles st.board.squares
        

    let findWordOnEmptyBoard (st: State.state)=
        next (0,0) st.dict st.hand true (Some []) false (0,0) true st.tileLookup st.placedTiles st.board.squares

    (*First try to find moves horizontally then if no move was found, try finding a word vertically*)
    let nextMove (st: State.state) : Move =
        if st.placedTiles.IsEmpty then
            match findWordOnEmptyBoard st with
            | Some move -> move
            | None -> [] // failwith "Did not find a starting word!"
        else
            let findMoveHorizontal = Map.tryPick (fun coord _ -> (initializeSearch coord st true)) st.placedTiles
            match findMoveHorizontal with
            | Some move -> move
            | None ->
                let findMoveVertical = Map.tryPick (fun coord _ -> (initializeSearch coord st false)) st.placedTiles
                match findMoveVertical with
                | Some move -> move
                | None -> [] // failwith "not implemented" // swap out tiles



// iterate over all tiles on the board (using tryPick)
// go through the dictionary starting from the tile
// test if a node containing "isWord" can be reached given
// the tiles on the board and on the hand
// test both vertically and horizontally
// if a word is found using only tiles on the board
// (non from the hand) then the move is not valid
// return the first valid move found
// if no valid move is found, give up all tiles
