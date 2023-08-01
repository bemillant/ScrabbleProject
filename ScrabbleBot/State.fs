module State

    open ScrabbleUtil
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          numberOfPlayers: uint32
          playerNumber: uint32
          currentPlayer: uint32
          hand: MultiSet.MultiSet<uint32>
          placedTiles: Map<coord, uint32 * (char * int)>
          tileLookup: Map<uint32, tile> }

    let mkState board dict numberOfPlayers playerNumber currentPlayer hand placedTiles tiles =
        { board = board
          dict = dict
          numberOfPlayers = numberOfPlayers
          playerNumber = playerNumber
          currentPlayer = currentPlayer
          hand = hand
          placedTiles = placedTiles
          tileLookup = tiles }

    let board st = st.board
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let hand st = st.hand