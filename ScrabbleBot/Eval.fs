module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello : (char * int) list =
        let h = ('H', 4)
        let e = ('E', 1)
        let l = ('L', 1)
        let o = ('O', 1)
        [h; e; l; l; o]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a:SM<int>) (b:SM<int>) : SM<int> =
        a >>= (fun aValue -> b >>= (fun bValue -> ret (aValue + bValue)))
        
    let div (a:SM<int>) (b:SM<int>) : SM<int> =
        a >>= (fun aValue -> b >>= (fun bValue ->
            if bValue = 0 then fail DivisionByZero
            else ret (aValue / bValue)))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    // lav en function (SM<int>) ud fra aExp som tager et state som input og outputter et result (success eller fail)
    // Output SM'en bliver pipet til funktionen evalSM som også får en state som input
    // Udregningen skal ikke ske før evalSM bliver kaldt, men funktionen skal være klar.
    // Hvis man gerne vil dele 6 med 2 skal der ikke passes en SM med tallet 3,
    // men en udregning/funktion som tager et state og outputter et result
    // Hvis jeg altid bare udregner et tal, og feeder til ret, kan jeg aldrig rejse en fail.
    // >>= kan bruges til at hive værdien ud af en SM og bruge den i en funktion som skaber en ny SM
    // >>= tager som argument SM<'a> og en funktion som mappper 'a til SM<'b>
    // >>>= bruger man hvis man har en SM<unit> og gerne vil mappe den til en SM<'a>
    
    // >>= tager en state monad, hiver dens 'a værdi ud og bruger den i en funktion som tager en 'a og returnerer en state monad<'a>. (Se PV pos)
    let rec arithEval (aExp:aExp) : SM<int> =
        let calc f a b : SM<int> = arithEval a >>= fun aValue -> arithEval b >>= fun bValue -> ret (f aValue bValue) 
        match aExp with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV pos -> arithEval pos >>= pointValue
        | Add (a, b) ->
            calc (+) a b
        | Sub (a, b) ->
            calc (-) a b
        | Mul (a, b) ->
            calc (*) a b
        | Div (a, b) ->
            boolEval (b .=. N 0) >>= (fun exp ->
                if exp then fail DivisionByZero
                else calc (/) a b
                )
        | Mod (a, b) ->
            boolEval (b .=. N 0) >>= (fun exp ->
                if exp then fail DivisionByZero
                else calc (%) a b
                )
        | CharToInt cExp ->
            let character = charEval cExp
            character >>= fun c -> ret (int c)
    and charEval (cExp:cExp) : SM<char> =
        match cExp with
        | C c -> ret c
        | CV pos -> arithEval pos >>= characterValue
        | ToUpper c -> charEval c >>= fun c -> ret (System.Char.ToUpper c)
        | ToLower c -> charEval c >>= fun c -> ret (System.Char.ToLower c)
        | IntToChar a -> arithEval a >>= fun a -> ret (char a)
    and boolEval (bExp:bExp) : SM<bool> =
        let compare f a b : SM<bool> = arithEval a >>= fun aValue -> arithEval b >>= fun bValue -> ret (f aValue bValue)
        match bExp with
        | TT -> ret true (* true *)
        | FF -> ret false (* false *)
        | AEq (a, b) -> compare (=) a b (* numeric equality *)
        | ALt (a, b) -> compare (<) a b (* numeric less than *)
        | Not bExp -> boolEval bExp >>= fun bValue -> ret (not bValue) (* boolean not *)
        | Conj (a, b) -> boolEval a >>= fun aValue -> (boolEval b) >>= fun bValue -> ret (aValue && bValue) (* boolean conjunction *)
        | IsDigit c -> charEval c >>= fun cValue -> ret (System.Char.IsDigit cValue) (* check for digit *)
        | IsLetter c -> charEval c >>= fun cValue -> ret (System.Char.IsLetter cValue) (* check for letter *)
        | IsVowel c -> (* check for vowel *)
            (charEval c) >>= (fun cValue ->
                let letter = System.Char.ToLower cValue
                match letter with
                | 'a' | 'e' | 'i' | 'o' | 'u' -> ret true
                | _ -> ret false)
            


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare x -> declare x
        | Ass (k, v) -> arithEval v >>= update k 
        | Skip -> ret ()
        | Seq (s1, s2) -> stmntEval s1 >>>= stmntEval s2
        | ITE (b, s1, s2) ->
            push >>>=
            boolEval b >>= fun bValue ->
                if bValue then stmntEval s1
                else stmntEval s2
            >>>= pop
        | While (b, s) ->
            push >>>=
            boolEval b >>= fun bValue ->
                if bValue then stmntEval s >>>= stmntEval (While (b, s))
                else ret ()
            >>>= pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    // let rec arithEval2 (aExp:aExp) : SM<int> =
    //     let calc f a b : SM<int> = arithEval2 a >>= fun aValue -> arithEval2 b >>= fun bValue -> ret (f aValue bValue) 
    //     let divByZeroCheck b f = boolEval2 (b .=. N 0) >>= fun isZero -> if isZero then fail DivisionByZero else f
    //     match aExp with
    //     | N n -> StateBuilder().Return n
    //     | V v -> StateBuilder().ReturnFrom lookup v
    //     | WL -> StateBuilder().ReturnFrom wordLength
    //     | PV pos -> StateBuilder().Bind(arithEval2 pos, pointValue)
    //     | Add (a, b) -> StateBuilder().ReturnFrom calc (+) a b 
    //     | Sub (a, b) -> StateBuilder().ReturnFrom calc (-) a b 
    //     | Mul (a, b) -> StateBuilder().ReturnFrom calc (*) a b
    //     | Div (a, b) -> StateBuilder().ReturnFrom divByZeroCheck b (calc (/) a b) 
    //     | Mod (a, b) -> StateBuilder().ReturnFrom divByZeroCheck b (calc (%) a b)
    //     | CharToInt cExp -> StateBuilder().Bind(charEval2 cExp, fun charVal -> ret (int charVal)) 
    // and charEval2 (cExp:cExp) : SM<char> =
    //     match cExp with
    //     | C c -> StateBuilder().Return c
    //     | CV pos -> StateBuilder().Bind(arithEval2 pos, characterValue)
    //     | ToUpper c -> StateBuilder().Bind(charEval2 c, (fun c -> ret (System.Char.ToUpper c)))
    //     | ToLower c -> StateBuilder().Bind(charEval2 c, (fun c -> ret (System.Char.ToLower c)))
    //     | IntToChar aExp -> StateBuilder().Bind(arithEval2 aExp, fun intVal -> ret (char intVal)) 
    // and boolEval2 (bExp:bExp) : SM<bool> =
    //     let compare f a b : SM<bool> = arithEval a >>= fun aValue -> arithEval b >>= fun bValue -> ret (f aValue bValue)
    //     match bExp with
    //     | TT -> StateBuilder().Return true
    //     | FF -> StateBuilder().Return false
    //     | AEq (a, b) -> StateBuilder().ReturnFrom compare (=) a b
    //     | ALt (a, b) -> StateBuilder().ReturnFrom compare (<) a b
    //     | Not bExp -> StateBuilder().Bind(boolEval2 bExp, fun bValue -> ret (not bValue))
    //     | Conj (a, b) -> StateBuilder().Combine(boolEval2 a, boolEval2 b) // Ikke rigtig
    //     | IsDigit c -> StateBuilder().Bind(charEval c, fun cValue -> ret (System.Char.IsDigit cValue))
    //     | IsLetter c -> StateBuilder().Bind(charEval c, fun cValue -> ret (System.Char.IsLetter cValue))
    //     | IsVowel c -> StateBuilder().Bind(charEval c, (fun cValue ->
    //             let letter = System.Char.ToLower cValue
    //             match letter with
    //             | 'a' | 'e' | 'i' | 'o' | 'u' -> ret true
    //             | _ -> ret false))
    // let stmntEval2 (stm:stm) : SM<unit> =
    //     match stm with
    //     | Declare x -> declare x
    //     | Ass (k, v) -> StateBuilder().Bind(arithEval2 v, update k)
    //     | Skip -> StateBuilder().Return()
    //     | Seq (s1, s2) -> StateBuilder().ReturnFrom(stmntEval s1 >>>= stmntEval s2)
    //     | ITE (b, s1, s2) -> StateBuilder().ReturnFrom (
    //         push >>>=
    //         StateBuilder().Bind(boolEval2 b, (fun bValue ->
    //             if bValue then stmntEval s1
    //             else stmntEval s2)
    //         )
    //         >>>= pop)
    //     | While (b, s) -> StateBuilder().ReturnFrom (
    //         push >>>=
    //         StateBuilder().Bind(boolEval2 b, (fun bValue ->
    //             if bValue then stmntEval s >>>= stmntEval (While (b, s))
    //             else StateBuilder().Return ()
    //         ))
    //         >>>= pop)

    let rec arithEval2 a =
        match a with
        | N n -> prog { return n }
        | V s -> prog { return! lookup s }
        | WL -> wordLength
        | PV n ->
            prog {
                let! x = arithEval2 n
                return! (pointValue x)
            }
        | Add(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b
                return (x + y)
            }
        | Sub(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b
                return (x - y)
            }
        | Mul(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b
                return (x * y)
            }
        | Div(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b

                if y <> 0 then
                    return (x / y)
                else
                    return! (fail (DivisionByZero))
            }
        | Mod(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b

                if y <> 0 then
                    return (x % y)
                else
                    return! (fail (DivisionByZero))
            }
        | CharToInt c ->
            prog {
                let! x = charEval2 c
                return (int (x))
            }
    and charEval2 c =
        match c with
        | C c -> prog { return c }
        | CV a ->
            prog {
                let! x = arithEval2 a
                return! characterValue x
            }
        | ToLower c ->
            prog {
                let! x = charEval2 c
                return System.Char.ToLower x
            }
        | ToUpper c ->
            prog {
                let! x = charEval2 c
                return System.Char.ToUpper x
            }
        | IntToChar a ->
            prog {
                let! x = arithEval2 a
                return (char (x))
            }

    let rec boolEval2 b =
        match b with
        | TT -> prog { return true }
        | FF -> prog { return false }
        | AEq(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b
                return (x = y)
            }
        | ALt(a, b) ->
            prog {
                let! x = arithEval2 a
                let! y = arithEval2 b
                return (x < y)
            }
        | Conj(a, b) ->
            prog {
                let! x = boolEval2 a
                let! y = boolEval b
                if x && y then return true else return false
            }
        | IsLetter c ->
            prog {
                let! x = charEval2 c
                return (System.Char.IsLetter(x))
            }
        | IsDigit c ->
            prog {
                let! x = charEval2 c
                return (System.Char.IsDigit(x))
            }
        | IsVowel c ->
            prog {
                let! x = charEval2 c

                match System.Char.ToLower(x) with
                | 'a'
                | 'e'
                | 'i'
                | 'o'
                | 'u' -> return true
                | _ -> return false
            }
        | Not b ->
            prog {
                let! x = boolEval2 b

                match x with
                | true -> return! boolEval2 FF
                | false -> return! boolEval2 TT
            }

    let rec stmntEval2 stmnt =
        match stmnt with
        | Declare s -> prog { return! declare s }
        | Ass(s, a) ->
            prog {
                let! x = arithEval2 a
                return! update s x
            }
        | Skip -> prog { return! stmntEval2 stmnt }
        | Seq(stmt1, stmt2) ->
            prog {
                do! stmntEval2 stmt1
                do! stmntEval2 stmt2
            }
        | ITE(b, stmt1, stmt2) ->
            prog {
                let! x = boolEval2 b

                if x then
                    do! push
                    do! stmntEval2 stmt1
                    return! pop
                else
                    do! push
                    do! stmntEval2 stmt2
                    return! pop
            }
        | While(b, stmt) ->
            prog {
                let! x = boolEval2 b

                if x then
                    do! push
                    do! stmntEval2 stmt
                    do! stmntEval2 (While(b, stmt))
                    return! pop
                else
                    return! stmntEval2 stmt
            }

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun (stm:stm) : squareFun =
        let initialState w pos acc = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]
        fun w pos acc -> stmntEval2 stm >>>= (lookup "_result_") |> evalSM (initialState w pos acc)

    type square = Map<int, squareFun>
    type squareStmnt = Map<int, stm>
    let stmntsToSquare (m:squareStmnt) : square =
        Map.map (fun _ value -> stmntToSquareFun value) m
    type coord = int * int
    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun (stm:stm) (t:Map<int, 'a>) ((x,y):coord) : Result<'a option, Error> = // ((x,y):coord) tilføjet. Det stod ikke i opgaven.
        let initialState = mkState [("_x_", x); ("_y_", y); ("_result_", 0)] word.Empty ["_x_"; "_y_"; "_result_"]
        let result = stmntEval2 stm >>>= (lookup "_result_")
                     |> evalSM initialState
        match result with
        | Success res ->
            Success (Map.tryFind res t)
        | Failure error -> Failure error

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt (m:(int * stm) list) : board =
        {
            center = c
            defaultSquare = stmntToSquareFun defaultSq
            squares = stmntToBoardFun boardStmnt (stmntsToSquare (Map.ofList m))
        }
    