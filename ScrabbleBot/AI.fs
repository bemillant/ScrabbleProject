namespace Zyzzyva

open Parser
open ScrabbleUtil
open Dictionary
open StateMonad

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

    let updatedAccMove (accMove: Move option) (coord:coord) (tileId:uint32) (c:char) (p:int) = 
        accMove |> Option.get |> List.append [coord, (tileId, (c, p))] |> Some
            
    let rec next
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) (hasFoundWord:bool) // changes through recursion
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) (squares:boardFun2) // stays the same
        : Move option seq = // return type
        
        let isOutOfBounds =
            match squares coord with
            | Failure error -> true // Will never happen
            | Success square ->
                match square with
                | Some _ -> false
                | None -> true
                
        let boardTile coord : (uint32*(char*int)) option = placedTiles.TryFind coord
        
        let hasOrthogonalLetter = 
            let firstCoord = getNextCoord coord true (not isHorizontal)
            let secondCoord = getNextCoord coord false (not isHorizontal)
            match (boardTile firstCoord, boardTile secondCoord) with
            | None, None -> false
            | _, _ -> true
        match boardTile coord with
        | Some (id, (c, p)) -> seq { yield! tryBoardTile (id, (c, p)) coord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles squares }
        | None ->
            if hasFoundWord 
            then
                if isPrefixSearch
                then
                    let result = node |> reverse
                    match result with
                    | Some (_, reverseNode) ->
                        let nextCoord = getNextCoord anchorCoord false isHorizontal
                        seq { yield! next nextCoord reverseNode hand false accMove true anchorCoord isHorizontal idTileLookup placedTiles squares }
                    | None -> failwith "Not possible" 
                else
                    let move = accMove |> Option.get
                    if move.IsEmpty 
                    then seq { yield None }
                    else if placedTiles.IsEmpty //Only needed the for the first move
                        then 
                            if (move.Length < 3) 
                            then seq { yield! tryHand coord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles squares }
                            else seq { yield accMove }
                        else seq { yield accMove }
            else
                if isOutOfBounds
                then seq { yield None }
                else
                // if hasOrthogonalLetter // Only keep else part, if invalid moves are filtered away later
                // then 
                //     let result = node |> reverse
                //     match result with
                //     | Some (_, reverseNode) -> 
                //         let nextCoord = getNextCoord anchorCoord false isHorizontal
                //         seq { yield! next nextCoord reverseNode hand false accMove hasFoundWord anchorCoord isHorizontal idTileLookup placedTiles squares }
                //     | None -> seq { yield None }
                // else
                    seq { yield! tryHand coord node hand isPrefixSearch accMove anchorCoord isHorizontal idTileLookup placedTiles squares }
    and tryBoardTile
        ((id, (c, p)):(uint32*(char*int)))
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) 
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) (squares:Parser.boardFun2) 
        : Move option seq =
        
        let result = step c node
        match result with
        | Some (foundWord, node) -> 
            let nextCoord = getNextCoord coord isPrefixSearch isHorizontal
            let updatedMove = updatedAccMove accMove coord id c p
            seq { yield! next nextCoord node hand isPrefixSearch updatedMove foundWord anchorCoord isHorizontal idTileLookup placedTiles squares }
        | None -> seq { yield None }
        
    and tryHand
        (coord:coord) (node:Dict) (hand:MultiSet.MultiSet<uint32>) (isPrefixSearch:bool) (accMove:Move option) 
        (anchorCoord:coord) (isHorizontal:bool) (idTileLookup:Map<uint32, tile>) (placedTiles:Map<coord, uint32*(char*int)>) (squares:Parser.boardFun2) 
        : Move option seq =
        
        let useLetter (id, (c, p)) foundWord node =
            let nextCoord = getNextCoord coord isPrefixSearch isHorizontal
            let updatedMove = updatedAccMove accMove coord id c p
            let updatedHand = hand |> MultiSet.removeSingle id
            seq { yield! next nextCoord node updatedHand isPrefixSearch updatedMove foundWord anchorCoord isHorizontal idTileLookup placedTiles squares }

        let tryReverse =
            if isPrefixSearch
            then
                let result = reverse node
                match result with
                | Some (_, reverseNode) ->
                    let nextCoord = getNextCoord anchorCoord false isHorizontal
                    seq { yield! next nextCoord reverseNode hand false accMove false anchorCoord isHorizontal idTileLookup placedTiles squares }
                | None -> seq { yield None }
            else seq { yield None }

        let tryLetter (id, (c, p)) : Move option seq =
            let result = step c node
            match result with
            | Some (foundWord, node) -> useLetter (id, (c, p)) foundWord node
            | None -> tryReverse
            
        let tryTileId id = idTileLookup.[id] |> Set.toList |> List.collect (fun (c, p) -> tryLetter (id, (c, p)) |> Seq.toList)
        hand |> MultiSet.toList |> List.collect (fun id -> tryTileId id) |> List.toSeq
        
    let initializeSearch (anchorCoord: coord) (st: State.state) (isHorizontal: bool) : Move option seq =
        next anchorCoord st.dict st.hand true (Some []) false anchorCoord isHorizontal st.tileLookup st.placedTiles st.board.squares
            
    let points (st:State.state) (move:Move) : int =
        let convert ((coord, (id, (c, p))):PlayedTile) = (coord, (c, p))
        let coordAndWord =
            move
            |> List.map convert
            |> List.sortBy fst
        let word = coordAndWord |> List.map snd
        let getSquareFromBoard coord =
            let result = st.board.squares coord
            match result with
            | Success squareOption ->
                match squareOption with
                | Some square -> square
                | None -> st.board.defaultSquare
            | Failure error -> failwith "error"
        let getSquare coord =
            match st.placedTiles.TryFind coord with
            | Some _ -> st.board.defaultSquare
            | None -> getSquareFromBoard coord
        let toSquareList (square:square) = square |> Map.toList
        let squareFunctions =
            coordAndWord
            |> List.map (fun (coord, _) -> getSquare coord)
            |> List.map toSquareList
            |> List.collect (List.mapi (fun idx squareFunList -> (idx, squareFunList)))
        let sortedFunctions =
            squareFunctions
            |> List.sortBy (fun (idx, (priority, squareFunction)) -> priority)
        let accumulatePoints (f:squareFun) (idx:int) (acc:int) : int =
            let result = f word idx acc
            match result with
            | Success points -> points
            | Failure error -> failwith "error"
        sortedFunctions
        |> List.fold (fun acc (idx, (priority, squareFunction)) -> accumulatePoints squareFunction idx acc) 0
        
    
    // Collect all boardfunctions with the corrosponding coords and collect their squares
    // If the square option is None, then use the default square
    // This will result in a collection of squares
    // Also extract the word from the move. We will also need the index of each letter
    // Then calculate the points generated from the word using a folding function
    // respecting the priority of each calculation from each square
    
    let bestMoveFromList (st:State.state) (list:Move list) : Move option =
        match list with
        | [] -> None
        | list ->
            list
            |> List.maxBy (points st)
            |> Some

    let isValid (move:Move) (st:State.state) : bool = // is true when no other invalid words are created around the move
        let searchHorizontal =
            match move with
            | [] -> true // Should never happen
            | [x] -> true // Should never happen
            | tileOne::tileTwo::tail -> snd (fst tileOne) = snd (fst tileTwo) // Do they share y-coordinates
        let orthogonalWordIsValid ((coord, (_, (c, _))):PlayedTile) =
            let orthogonalWord =
                let prefix =
                    let rec generatePrefix coord =
                        let previousCoordinate =
                            if searchHorizontal
                            then (fst coord - 1, snd coord)
                            else (fst coord, snd coord - 1)
                        match st.placedTiles.TryFind previousCoordinate with
                        | Some (id, (c, p)) -> generatePrefix previousCoordinate + string c
                        | None -> ""
                    generatePrefix coord
                let suffix =
                    let rec generateSuffix coord =
                        let nextCoordinate =
                            if searchHorizontal
                            then (fst coord + 1, snd coord)
                            else (fst coord, snd coord + 1)
                        match st.placedTiles.TryFind nextCoordinate with
                        | Some (id, (c, p)) -> string c + generateSuffix nextCoordinate
                        | None -> ""
                    generateSuffix coord
                prefix + string c + suffix
            if orthogonalWord.Length = 1
            then true
            else Dictionary.lookup orthogonalWord st.dict
        move |> List.forall orthogonalWordIsValid
        
    let bestMoveOnTile (coord:coord) (st:State.state) : Move option =
        let horizontalMoves = initializeSearch coord st true |> Seq.toList
        let verticalMoves   = initializeSearch coord st false |> Seq.toList
        let allMoves        = horizontalMoves @ verticalMoves
                              |> List.filter Option.isSome
                              |> List.map Option.get
        let filteredMoves   = allMoves |> List.filter (fun move -> isValid move st)
        bestMoveFromList st filteredMoves
    
    let bestMoves (st: State.state) =
        st.placedTiles
        |> Map.toList
        |> List.choose (fun (coord, _) -> (bestMoveOnTile coord st))
        
    let bestMove (st: State.state) : Move option =
        match bestMoves st with
        | [] -> None
        | moves -> bestMoveFromList st moves
        
    let findWordOnEmptyBoard (st: State.state) = initializeSearch (0,0) st true

    let moveWithoutAlreadyPlacedTiles (st: State.state) (move:Move) : Move =
        let isEmpty coord = not (st.placedTiles.ContainsKey coord)
        move |> List.filter (fun (coord, (id, (c, p))) -> isEmpty coord)
    
    let nextMove (st: State.state) : Move =
        if st.placedTiles.IsEmpty then
            match bestMoveOnTile (0,0) st with
            | Some move -> move
            | None -> [] // failwith "Did not find a starting word!"
        else
            match bestMove st with
            | Some move -> moveWithoutAlreadyPlacedTiles st move
            | None -> []



// iterate over all tiles on the board (using tryPick)
// go through the dictionary starting from the tile
// test if a node containing "isWord" can be reached given
// the tiles on the board and on the hand
// test both vertically and horizontally
// if a word is found using only tiles on the board
// (non from the hand) then the move is not valid
// return the first valid move found
// if no valid move is found, give up all tiles
