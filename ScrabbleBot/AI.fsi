namespace Zyzzyva

open System.Threading
open ScrabbleUtil

module AI =
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list

    val nextMove: State.state -> Async<Move>
    val initializeSearch: (int * int) -> State.state -> bool -> seq<Move option>
    val getNextCoord: coord -> bool -> bool -> coord
    val updateAcc : Move option -> coord -> (uint32* (char*int)) -> Move option
    val next: (int * int) -> Dictionary.Dict -> MultiSet.MultiSet<uint32> -> bool -> Move option -> bool -> (int * int) -> bool -> Map<uint32,tile> -> Map<coord,(uint32 * (char * int))> -> Parser.boardFun2 -> seq<Move option>