namespace Zyzzyva

open ScrabbleUtil

module AI =
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list

    val nextMove: State.state -> Move
    val extract: 'a * ('b * 'c) -> 'b
    val findMoveFromTile: coord -> State.state -> bool -> Move option
    val nextCoord: coord -> bool -> bool -> coord ->coord

    val updateAcc : Move option -> coord -> (uint32* (char*int)) -> Move option
    val buildWord:
        uint32 -> coord -> Dictionary.Dict -> Move option -> MultiSet.MultiSet<uint32> -> bool -> Map<uint32,tile> -> bool -> coord -> Move option
