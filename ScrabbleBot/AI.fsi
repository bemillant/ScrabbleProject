namespace Zyzzyva

open ScrabbleUtil

module AI =
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list

    val nextMove: State.state -> Move
    val initializeSearch: coord -> State.state -> bool -> Move option
    val getNextCoord: coord -> bool -> bool -> coord
    val updateAcc : Move option -> coord -> (uint32* (char*int)) -> Move option
    val next: coord -> Dictionary.Dict -> MultiSet.MultiSet<uint32> -> bool -> Move option -> bool -> coord -> bool -> Map<uint32,tile> -> Map<coord,(uint32 * (char * int))> -> Parser.boardFun2 -> Move option