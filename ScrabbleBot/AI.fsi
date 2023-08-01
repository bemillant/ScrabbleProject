namespace Zyzzyva

module AI =
    open ScrabbleUtil
    val testBool : bool
    val testInt : int
    
    type PlayedTile = coord * (uint32 * (char * int))

    type Move = PlayedTile list
