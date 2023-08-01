
module State
    open ScrabbleUtil
    
    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          numberOfPlayers: uint32
          playerNumber: uint32
          currentPlayer: uint32
          hand: MultiSet.MultiSet<uint32>
          placedTiles: Map<coord, uint32 * (char * int)>
          tileLookup: Map<uint32, tile> }
    
    val mkState : Parser.board -> Dictionary.Dict -> uint32 -> uint32 -> uint32 -> MultiSet.MultiSet<uint32> -> Map<coord,(uint32 * (char * int))> -> Map<uint32,tile> -> state

    val board : state -> Parser.board
    val dict : state -> Dictionary.Dict
    val playerNumber : state -> uint32
    val hand : state -> MultiSet.MultiSet<uint32>
    
