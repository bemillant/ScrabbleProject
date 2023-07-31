module Dictionary
    type Gaddag
    val empty: unit -> Gaddag
    val insert: string -> Gaddag -> Gaddag
    val step: char -> Gaddag -> (bool * Gaddag) option
    val reverse: Gaddag -> (bool * Gaddag) option
    val lookup: string -> Gaddag -> bool
    val print: Gaddag -> unit
    val nodeCount: Gaddag -> int
    val maxDepth: Gaddag -> int