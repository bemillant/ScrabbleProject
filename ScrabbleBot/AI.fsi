namespace Zyzzyva

open ScrabbleUtil

module AI =
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list

    val nextMove: State.state -> Move
    val findMoveFromTile: coord -> State.state -> bool -> Move option
    val nextCoord: coord -> bool -> coord
    val buildWord: uint32 -> Dictionary.Dict -> Move option -> MultiSet.MultiSet<uint32> -> bool -> Move option
