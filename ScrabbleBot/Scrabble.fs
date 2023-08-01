namespace Zyzzyva

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

        let nextPlayer numberOfPlayers currentPlayer = (currentPlayer % numberOfPlayers) + 1u

        let placeTiles (playedTiles: Move) (placedTiles: Map<coord, uint32 * (char * int)>) =
            List.fold (fun acc (coord, (id, (c, p))) -> Map.add coord (id, (c, p)) acc) placedTiles playedTiles

        let getRidOfTiles (playedTiles: Move) (hand: MultiSet.MultiSet<uint32>) =
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

        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        // let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () ->
            playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn handSet Map.empty tiles)
