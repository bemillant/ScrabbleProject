namespace Zyzzyva

open System.Threading.Tasks
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint
open AI

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
    
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let getTimeout timeout = 
            match timeout with
            |Some t -> t
            |None -> 5000u
        
        let findPass playerId= 
                match st.playerPassCounter.TryFind playerId with
                    |None ->  st.playerPassCounter |> Map.add playerId 1u
                    |Some v -> st.playerPassCounter |> Map.add playerId  (v + 1u)

        let nextPlayer numberOfPlayers currentPlayer = (currentPlayer % numberOfPlayers) + 1u

        let placeTiles (playedTiles: Move) (placedTiles: Map<coord, uint32 * (char * int)>) =
            List.fold (fun acc (coord, (id, (c, p))) -> Map.add coord (id, (c, p)) acc) placedTiles playedTiles

        let getRidOfTiles (swappedOutTiles: uint32 list) (hand: MultiSet.MultiSet<uint32>) =
            swappedOutTiles |> List.fold (fun acc id -> MultiSet.removeSingle id acc) hand
            
        let getRidOfPlayedTiles (playedTiles: Move) (hand: MultiSet.MultiSet<uint32>) =
            let ids = playedTiles |> List.map (fun (_, (id, (_, _))) -> id)
            getRidOfTiles ids hand

        let addNewTiles (newPieces: (uint32 * uint32) list) (hand: MultiSet.MultiSet<uint32>) =
            newPieces |> List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) hand

        let swapPieces (st: State.state) : uint32 list =
            let piecesLeft = st.tilesLeft
            if piecesLeft = 0u 
            then [] 
            else
                let amountToSwap = int (min 7u piecesLeft)
                st.hand |> MultiSet.toList |> List.take amountToSwap
        
        let rec aux (st: State.state) =
            let swappedPieces = swapPieces st
            if st.currentPlayer = st.playerNumber then
                //Print.printHand pieces (State.hand st)
                
                let timeoutMilliseconds = int (getTimeout st.timeout)
                let timeOutTask = Task.Delay(timeoutMilliseconds)
                let findMoveTask = Async.StartAsTask (nextMove st)
                let emptyMove : Async<Move> =
                    async {
                        return []
                    }
                let emptyMoveTask = Async.StartAsTask emptyMove
                
                let result =
                    async {
                        let! completedTask = Async.AwaitTask (Task.WhenAny(findMoveTask, timeOutTask))
                        if completedTask = findMoveTask then
                            return findMoveTask
                        else
                            return emptyMoveTask
                    }
                let move = Async.RunSynchronously(result)
                
                //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                match move.Result with
                | [] -> match swappedPieces with 
                        | [] -> send cstream (SMPass) 
                        | _ -> send cstream (SMChange swappedPieces) 
                | _ -> send cstream (SMPlay move.Result)

            let msg = recv cstream

            match msg with
            | RCM(CMPlaySuccess(playedTiles, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    {
                    board = st.board
                    dict = st.dict
                    numberOfPlayers = st.numberOfPlayers
                    playerNumber = st.playerNumber
                    currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                    hand = st.hand |> getRidOfPlayedTiles playedTiles |> addNewTiles newPieces
                    placedTiles = st.placedTiles |> placeTiles playedTiles
                    tileLookup = st.tileLookup
                    tilesLeft = st.tilesLeft - (uint32 playedTiles.Length)
                    timeout = st.timeout
                    playerPassCounter = st.playerPassCounter
                    }
                    : State.state

                aux st'
            | RCM(CMPlayed(playerId, playedTiles, points)) ->
                (* Successful play by other player. Update your state *)
                let st' =
                    {
                    board = st.board
                    dict = st.dict
                    numberOfPlayers = st.numberOfPlayers
                    playerNumber = st.playerNumber
                    currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                    hand = st.hand
                    placedTiles = st.placedTiles |> placeTiles playedTiles
                    tileLookup = st.tileLookup
                    tilesLeft = st.tilesLeft - (uint32 playedTiles.Length)
                    timeout = st.timeout
                    playerPassCounter = st.playerPassCounter |> Map.add playerId 0u
                    }
                    : State.state

                aux st'
            | RCM(CMPlayFailed(playerId, playedTiles)) ->
                (* Failed play. Update your state *)
                let st' =
                    {
                    board = st.board
                    dict = st.dict
                    numberOfPlayers = st.numberOfPlayers
                    playerNumber = st.playerNumber
                    currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                    hand = st.hand
                    placedTiles = st.placedTiles
                    tileLookup = st.tileLookup
                    tilesLeft = st.tilesLeft
                    timeout = st.timeout
                    playerPassCounter = st.playerPassCounter
                    }
                    : State.state
                aux st'
            | RCM (CMChangeSuccess newPieces) -> 
                (* Swap pieces in hand *)
                let st' = 
                    {
                        board = st.board
                        dict = st.dict
                        numberOfPlayers = st.numberOfPlayers
                        playerNumber = st.playerNumber
                        currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                        hand = st.hand |> getRidOfTiles swappedPieces |> addNewTiles newPieces
                        placedTiles = st.placedTiles
                        tileLookup = st.tileLookup
                        tilesLeft = st.tilesLeft - (uint32 newPieces.Length)
                        timeout = st.timeout
                        playerPassCounter = st.playerPassCounter 
                    }
                    : State.state
                aux st'
            | RCM (CMChange (playerID, numOfSwap)) -> 
                (* Another player swapped tiles *)
                let st' = 
                    {
                        board = st.board
                        dict = st.dict
                        numberOfPlayers = st.numberOfPlayers
                        playerNumber = st.playerNumber
                        currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                        hand = st.hand
                        placedTiles = st.placedTiles
                        tileLookup = st.tileLookup
                        tilesLeft = st.tilesLeft - numOfSwap
                        timeout = st.timeout
                        playerPassCounter = st.playerPassCounter 
                    }
                    : State.state
                aux st'
            |RGPE ([GPENotEnoughPieces(changeTiles, availableTiles)]) -> 
                let st' = 
                    {
                        board = st.board
                        dict = st.dict
                        numberOfPlayers = st.numberOfPlayers
                        playerNumber = st.playerNumber
                        currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                        hand = st.hand
                        placedTiles = st.placedTiles
                        tileLookup = st.tileLookup
                        tilesLeft = availableTiles
                        timeout = st.timeout
                        playerPassCounter = st.playerPassCounter 
                    }
                    : State.state
                aux st'
            | RCM(CMGameOver p) ->
                printfn "Game finished!"
            | RCM(CMForfeit playerId) ->
                let st' =
                    {
                    board = st.board
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
                    tileLookup = st.tileLookup
                    tilesLeft = st.tilesLeft
                    timeout = st.timeout
                    playerPassCounter = st.playerPassCounter
                    }
                    : State.state

                aux st'
            | RCM(CMPassed playerId) ->
                let st' =
                    {
                    board = st.board
                    dict = st.dict
                    numberOfPlayers = st.numberOfPlayers
                    playerNumber = st.playerNumber
                    currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                    hand = st.hand
                    placedTiles = st.placedTiles
                    tileLookup = st.tileLookup
                    tilesLeft = st.tilesLeft
                    timeout = st.timeout
                    playerPassCounter = findPass playerId
                    }
                    : State.state
                aux st'
            | RCM(CMTimeout playerId) -> 
                let st' =
                    {
                    board = st.board
                    dict = st.dict
                    numberOfPlayers = st.numberOfPlayers
                    playerNumber = st.playerNumber
                    currentPlayer = nextPlayer st.numberOfPlayers st.currentPlayer
                    hand = st.hand
                    placedTiles = st.placedTiles
                    tileLookup = st.tileLookup
                    tilesLeft = st.tilesLeft
                    timeout = st.timeout
                    playerPassCounter = findPass playerId
                    }
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

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        // let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () ->
            playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn handSet Map.empty tiles timeout)
