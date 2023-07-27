﻿namespace Zyzzyva

open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        placedTiles   : Map<coord, uint32>
    }

    let mkState board dict playerNumber hand placedTiles =
        {
            board = board
            dict = dict
            playerNumber = playerNumber
            hand = hand
            placedTiles = placedTiles
        }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading
    type PlayedTile = coord * (uint32 * (char * int))
    
    let playGame cstream pieces (st : State.state) =
        
        let placeTiles (playedTiles:PlayedTile list) (placedTiles:Map<coord, uint32>) =
            List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) placedTiles playedTiles
            
        let getRidOfTiles (playedTiles:PlayedTile list) (hand:MultiSet.MultiSet<uint32>) =
            playedTiles |> List.fold (fun acc (_, (id, (_, _))) -> MultiSet.removeSingle id acc) hand
            
        let addNewTiles (newPieces:(uint32 * uint32) list) (hand:MultiSet.MultiSet<uint32>) =
            newPieces |> List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) hand

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(playedTiles, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    {
                        board = st.board
                        dict = st.dict
                        playerNumber = st.playerNumber
                        hand = st.hand |> getRidOfTiles playedTiles |> addNewTiles newPieces
                        placedTiles = st.placedTiles |> placeTiles playedTiles
                    } : State.state
                aux st'
            | RCM (CMPlayed (playerId, playedTiles, points)) ->
                (* Successful play by other player. Update your state *)
                let st' =
                    {
                        board = st.board
                        dict = st.dict
                        playerNumber = st.playerNumber
                        hand = st.hand
                        placedTiles = st.placedTiles |> placeTiles playedTiles
                    } : State.state
                aux st'
            | RCM (CMPlayFailed (playerId, playedTiles)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg)                (* Scrabble board *)
            (dictf : bool -> Dictionary.Dict)   (* Dictionary (call with true if using a GADDAG, and false if using a Trie) *)
            (numPlayers : uint32)               (* Number of players *)
            (playerNumber : uint32)             (* Your player number *)
            (playerTurn  : uint32)              (* starting player number *)
            (hand : (uint32 * uint32) list)     (* starting hand (tile id, number of tiles) *)
            (tiles : Map<uint32, tile>)         (* Tile lookup table *)
            (timeout : uint32 option)           (* Timeout in miliseconds *)
            (cstream : Stream) =                (* Communication channel to the server *)
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
        