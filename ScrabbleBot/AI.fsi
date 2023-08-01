namespace Zyzzyva
open ScrabbleUtil

module AI =
    type PlayedTile = coord * (uint32 * (char * int))
    type Move = PlayedTile list
    
    val nextMove : State.state -> Move