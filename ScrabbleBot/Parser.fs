// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. (spaces >>. p2)
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> (spaces >>. p2)
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. (spaces >>. p2)
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let pid = pchar '_' <|> pletter .>>. many palphanumeric |>> (fun (c, lst) ->
        let rec charListToString lst =
            match lst with
            | [] -> ""
            | x::xs -> string x + charListToString xs
        string c + charListToString lst)
    
    let unop op a = op .>*>. a |>> snd
    let binop op p1 p2 = p1 .>*>. op .>*>. p2 |>> fun ((a, b), c) -> (a, c)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Variable"
    let WLParse   = pstring "wordLength" |>> (fun _ -> WL) <?> "Word Length"
    let NegationParse = unop (pchar '-') TermParse |>> (fun x -> Mul (N -1, x)) <?> "Negation"
    let PVParse   = pPointValue >*>. parenthesise TermParse |>> PV <?> "Point Value"
    let ParParse = parenthesise TermParse
    let CharToInt = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToInt; NegationParse; NParse; PVParse; VParse; ParParse]
    let AexpParse = TermParse 

    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let ToUpperParse = unop pToUpper (parenthesise CharParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop pToLower (parenthesise CharParse) |>> ToLower <?> "ToLower"
    let IntToChar = unop pIntToChar (parenthesise AtomParse) |>> IntToChar <?> "IntToChar"  
    do cref := choice [ToUpperParse; ToLowerParse; CVParse; CParse; IntToChar]

    let CexpParse = CharParse

    let JunctionParse, jref = createParserForwardedToRef<bExp>()
    let EqualityParse, eref = createParserForwardedToRef<bExp>()
    let BinaryParse, bref = createParserForwardedToRef<bExp>()
    let ConjunctionParse = binop (pstring "/\\") EqualityParse JunctionParse |>> Conj <?> "Conjunction"
    let DisjunctionParse = binop (pstring "\\/") EqualityParse JunctionParse |>> (fun (a, b) -> a .||. b) <?> "Conjunction"
    do jref := choice [ConjunctionParse; DisjunctionParse; EqualityParse]
    
    let EqualParse = binop (pchar '=') TermParse TermParse |>> AEq <?> "Equality"
    let NotEqualParse = binop (pstring "<>") TermParse TermParse |>> (fun (a, b) -> a .<>. b) <?> "Not Equal"
    let LessThanParse = binop (pchar '<') TermParse TermParse |>> ALt <?> "Less than"
    let GreaterThanParse = binop (pchar '>') TermParse TermParse |>> (fun (a, b) -> a .>. b) <?> "Greater than"
    let LessThanEqualParse = binop (pstring ">=") TermParse TermParse |>> (fun (a, b) -> a .>=. b) <?> "Less than or equal" 
    let GreaterThanEqualParse = binop (pstring "<=") TermParse TermParse |>> (fun (a, b) -> a .<=. b) <?> "Greater than or equal"
    do eref := choice [EqualParse; NotEqualParse; LessThanParse; GreaterThanParse; LessThanEqualParse; GreaterThanEqualParse; BinaryParse]
    
    let TrueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let NotParse = unop (pchar '~') JunctionParse |>> Not <?> "Not"
    let IsLetterParse = unop pIsLetter CexpParse |>> IsLetter <?> "Is letter"
    let IsVowelParse = unop pIsVowel CexpParse |>> IsVowel <?> "Is vowel"
    let IsDigitParse = unop pIsDigit CexpParse |>> IsDigit <?> "Is digit"
    let ParBinaryParse = parenthesise JunctionParse
    do bref := choice [TrueParse; FalseParse; NotParse; IsLetterParse; IsVowelParse; IsDigitParse; ParBinaryParse]
    let BexpParse = JunctionParse
    let StatementParse, sref = createParserForwardedToRef<stm>()
    let SequenceParse, sqref = createParserForwardedToRef<stm>()
    let SeparationParse = binop (pchar ';') StatementParse SequenceParse |>> Seq <?> "Separation"
    do sqref := choice [SeparationParse; StatementParse]
    let DeclareParse = pdeclare >>. spaces1 >>. pid  |>> Declare <?> "Declare"
    let AssignParse = binop (pstring ":=") pid AexpParse |>> Ass <?> "Assignment"
    let curly p = pchar '{' >*>. p .>*> pchar '}'
    let IfThenElseParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curly SequenceParse .>*> pelse .>*>. curly SequenceParse |>> (fun ((b, s1), s2) -> ITE (b, s1, s2)) <?> "If then else"
    let IfThenParse = (pif >*>. parenthesise BexpParse) .>*>. (pthen >*>. curly SequenceParse) |>> (fun (b, s1) -> ITE (b, s1, Skip)) <?> "If then"
    let WhileParse = (pwhile >*>. parenthesise BexpParse) .>*>. (pdo >*>. curly SequenceParse) |>> While <?> "While"
    do sref := choice [DeclareParse; AssignParse; IfThenElseParse; IfThenParse; WhileParse]
    let stmntParse = SequenceParse

    // Below is default template code
    
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let parseSquareProg (sqp:squareProg) : square =
        sqp |> Map.map (fun _ sourceCode -> stmntToSquareFun (getSuccess (run stmntParse sourceCode)))

    let parseBoardProg =
        fun s sqs -> stmntToBoardFun (getSuccess (run stmntParse s)) sqs
    let mkBoard (bp : boardProg) =
        let squareMap = bp.squares |> Map.map (fun k v -> parseSquareProg v)
        {
          center = bp.center
          defaultSquare = squareMap.[bp.usedSquare]
          squares = parseBoardProg bp.prog squareMap
        }
