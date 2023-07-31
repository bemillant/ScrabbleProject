namespace Zyzzyva

open Parser
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          numberOfPlayers: uint32
          playerNumber: uint32
          currentPlayer: uint32
          hand: MultiSet.MultiSet<uint32>
          placedTiles: Map<coord, uint32 * (char * int)>
          tileLookup: Map<uint32, tile> }

    let mkState board dict numberOfPlayers playerNumber currentPlayer hand placedTiles tiles =
        { board = board
          dict = dict
          numberOfPlayers = numberOfPlayers
          playerNumber = playerNumber
          currentPlayer = currentPlayer
          hand = hand
          placedTiles = placedTiles
          tileLookup = tiles }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand

module AI =
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list

    let nextMove (st: State.state) : Move =
        // Kald step med alle vores tilgængelig brikker eller reverse
        // Bliv ved indtil et ord et fundet

        // let a = reverse st.dict

        let findMoveFromTile (anchorCoord: coord) (st: State.state) (horizontal: bool) : Move option =
            let getTile coord = st.placedTiles.[coord] // Gets a placed tile from a coord in the format of a move. Check if the coord exists in the map first.
            let start = Some [ getTile anchorCoord ] // e.g. [A]

            let getChar ((_, (_, (c, _))): PlayedTile) = c

            let nextCoord ((x, y): coord) (prefixSearch: bool) =
                if prefixSearch then
                    if horizontal then (x - 1, y) else (x, y - 1)
                else if horizontal then
                    (x + 1, y)
                else
                    (x, y + 1)

            //Recursive function that attempts to build a word, tile by tile.
            //First step with a char and node and checks if this completes a word
            //If it does -> return acc word
            //If not:
            //If we get a sub-node from the step try again with a char from hand
            //If we do not get a sub-node (None) then call reverse
            //if reverse = Move -> move
            //elif reverse = None -> None
            let rec buildWord c node accMove hand hasBeenReversed : Move option =
                let check = step c node

                match check with
                | Some(b, _) when b -> accMove //If the we have found a word, simply return the accumilated word
                | Some(_, n) ->
                    match hand with
                    | [] -> None //If we have exhausted the hand then no word was found in this branch
                    | c :: restHand -> buildWord c n (accMove :: c) restHand false //We now try again with a tile from the hand. We append the tile to the wordBuilder-List (NEED TO FORMAT SUCH THAT IT FITS)
                | None ->
                    if hasBeenReversed then
                        None
                    else
                        let check = buildWord c (reverse node) accMove hand true

                        match check with
                        | Some move -> Some move
                        | None -> None


            //Try pick on handTiles


            //Recursive method to call after having called reverse
            //Maybe jump coord back to start?
            //Call step on (anchor?) node
            //If word is found -> Move
            //else
            //If we get a sub-node, call step again with a char
            //if we get None -> none

            // This may have a different signature.
            // State should probably not change during the function
            // let rec findMoveAux (coord:coord) (prefixSearch:bool) (st:State.state) (dict:Dict) (playedTiles:Move option) : Move option =
            //     if st.placedTiles.ContainsKey coord // Is there a tile already we can build off of?
            //     then // Maybe this should be its own function
            //         let addToMove tile = playedTiles |> Option.get |> List.append [tile] |> Some
            //         let tile = getTile coord
            //         let character = getChar tile
            //         let result = step character dict
            //         match result with
            //         | Some (true, _) -> addToMove tile // Success: word found
            //         | Some (false, node) -> failwith "not implemented"
            //         // Implement going further in gaddag. Call findMoveAux. Node must be used somewhere here. Call to reverse / step will be necessary.
            //         // While adding prefixes, all placed tiles in front of letter must be included successively.
            //         | None -> None
            //     else
            //         // Call findMoveAux (or some other method) with each tile in hand to see if a tile can be put here to form a word
            //         let availableTiles = failwith "not implemented" // This is a list of tiles (id, (char, point)) to used in the move.
            //         let validTiles =
            //             availableTiles
            //             |> List.map (fun (_, (c, _)) -> step c dict)
            //             |> List.filter (fun result ->
            //                 match result with
            //                 | Some _ -> true
            //                 | None -> false
            //                 ) // If the result finishes the word, the move should be instantly returned
            //         // availableTiles |> List.tryPick (findMoveAux (nextCoord prefixSearch) prefixSearch st dict validContinuation)
            //         failwith "not implemented"

            // findMoveAux anchorCoord true st st.dict start
            failwith "not implemented"

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

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let nextPlayer numberOfPlayers currentPlayer = (currentPlayer % numberOfPlayers) + 1u

        let placeTiles (playedTiles: AI.Move) (placedTiles: Map<coord, uint32 * (char * int)>) =
            let convertFromMoveToPlayedTiles playedTiles =
                List.map (fun (coord, (id, (c, p))) -> (coord, (id, c, p))) playedTiles

            List.fold (fun acc (coord, (id, (c, p))) -> Map.add coord (id, (c, p)) acc) placedTiles playedTiles

        let getRidOfTiles (playedTiles: AI.Move) (hand: MultiSet.MultiSet<uint32>) =
            playedTiles
            |> List.fold (fun acc (_, (id, (_, _))) -> MultiSet.removeSingle id acc) hand

        let addNewTiles (newPieces: (uint32 * uint32) list) (hand: MultiSet.MultiSet<uint32>) =
            newPieces |> List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) hand

        let rec aux (st: State.state) =

            if st.currentPlayer = st.playerNumber then
                Print.printHand pieces (State.hand st)
                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint
                    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                let input = System.Console.ReadLine()
                let move = RegEx.parseMove input

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (SMPlay move)

            let msg = recv cstream

            match msg with
            | RCM(CMPlaySuccess(playedTiles, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    { board = st.board
                      dict = st.dict
                      numberOfPlayers = st.numberOfPlayers
                      playerNumber = st.playerNumber
                      currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                      hand = st.hand |> getRidOfTiles playedTiles |> addNewTiles newPieces
                      placedTiles = st.placedTiles |> placeTiles playedTiles
                      tileLookup = st.tileLookup }
                    : State.state

                aux st'
            | RCM(CMPlayed(playerId, playedTiles, points)) ->
                (* Successful play by other player. Update your state *)
                let st' =
                    { board = st.board
                      dict = st.dict
                      numberOfPlayers = st.numberOfPlayers
                      playerNumber = st.playerNumber
                      currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                      hand = st.hand
                      placedTiles = st.placedTiles |> placeTiles playedTiles
                      tileLookup = st.tileLookup }
                    : State.state

                aux st'
            | RCM(CMPlayFailed(playerId, playedTiles)) ->
                (* Failed play. Update your state *)
                let st' =
                    { board = st.board
                      dict = st.dict
                      numberOfPlayers = st.numberOfPlayers
                      playerNumber = st.playerNumber
                      currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                      hand = st.hand
                      placedTiles = st.placedTiles
                      tileLookup = st.tileLookup }
                    : State.state

                aux st'
            | RCM(CMGameOver _) -> ()
            | RCM(CMForfeit playerId) ->
                let st' =
                    { board = st.board
                      dict = st.dict
                      numberOfPlayers = st.numberOfPlayers - 1u
                      playerNumber =
                        if st.playerNumber > playerId then
                            st.playerNumber - 1u
                        else
                            st.playerNumber
                      currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                      hand = st.hand
                      placedTiles = st.placedTiles
                      tileLookup = st.tileLookup }
                    : State.state

                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st


        aux st

    let startGame
        (boardP: boardProg) (* Scrabble board *)
        (dictf: bool -> Dictionary.Dict) (* Dictionary (call with true if using a GADDAG, and false if using a Trie) *)
        (numPlayers: uint32) (* Number of players *)
        (playerNumber: uint32) (* Your player number *)
        (playerTurn: uint32) (* starting player number *)
        (hand: (uint32 * uint32) list) (* starting hand (tile id, number of tiles) *)
        (tiles: Map<uint32, tile>) (* Tile lookup table *)
        (timeout: uint32 option) (* Timeout in miliseconds *)
        (cstream: Stream)
        = (* Communication channel to the server *)
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () ->
            playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn handSet Map.empty tiles)
