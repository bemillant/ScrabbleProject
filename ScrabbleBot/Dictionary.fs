module Dictionary
    type Key =
        | Character of char
        | None
    type Gaddag =
        {
            map : Map<Key, Gaddag>
            isEndOfWord : bool
        }
    type DoubleKeyList =
        {
            frontList : Key list
            endList : Key list
        }
    let empty () = { map = Map.empty; isEndOfWord = false }        
    let insert (str:string) (dict:Gaddag) : Gaddag =
        let rec insertKeyList keyList dict = 
            match keyList with
            | [] -> { map = dict.map; isEndOfWord = true }
            | [x] when x = Key.None -> { map = dict.map; isEndOfWord = true }
            | x::xs ->
                let map =
                    match x with
                    | x when Map.containsKey x dict.map -> dict.map.[x]
                    | _ -> empty ()
                let updatedChildDict = insertKeyList xs map
                let updatedMap = Map.add x updatedChildDict dict.map
                { map = updatedMap; isEndOfWord = dict.isEndOfWord }
        let permutations startingString : Key list list =
            let key k =
                if k = '#' then None
                else Character k
            let reverseString = startingString |> List.ofSeq |> List.rev |> List.map key
            let doubleKeyList = { frontList = reverseString; endList = [key '#'] }
            let moveFrontToBack doubleKeyList =
                match doubleKeyList.frontList with
                | [] -> { frontList = []; endList = [] } // Won't happen
                | frontHead::frontTail ->
                    let updatedEndList = 
                        match doubleKeyList.endList with
                        | [] -> [] // Won't happen
                        | hashTag::keys -> hashTag::frontHead::keys
                    { frontList = frontTail; endList = updatedEndList }
            let unfoldFunction =
                fun doubleKeyList ->
                    if doubleKeyList.frontList = [] then Option.None
                    else
                        let nextState = (moveFrontToBack doubleKeyList)
                        let keyList = doubleKeyList.frontList @ doubleKeyList.endList
                        Some (keyList, nextState)
            List.unfold unfoldFunction doubleKeyList
        List.fold (fun acc wordInKeys -> insertKeyList wordInKeys acc) dict (permutations str)
        // (HEY)
        // [ 
            // [Y E H #] // startingList
            // [E H # Y]
            // [H # E Y]
        // ]
        // (HELLO)
        // [ 
            // [O L L E H #] // startingList
            // [L L E H # O] 
            // [L E H # L O] 
            // [E H # L L O] 
            // [H # E L L O]
        // ]
        // (BLUD)
        // [ 
            // [D U L B #] // startingList
            // [U L B # D]
            // [L B # U D]
            // [B # L U D]
        // ]       

    let rec step (c:char) (dict:Gaddag) : (bool * Gaddag) option =
        let key c =
            match c with
            | '#' -> None
            | _ -> Character c
        if Map.containsKey (key c) dict.map then
            let dict2 = dict.map.[key c]
            Some (dict2.isEndOfWord, dict2)
        else Option.None
    
    let rec reverse (dict:Gaddag) : (bool * Gaddag) option =
        if Map.containsKey Key.None dict.map then
            let dict2 = dict.map.[Key.None]
            Some (dict2.isEndOfWord, dict2)
        else Option.None
        
    let lookup (str:string) (dict:Gaddag) : bool =
        let rec aux remainingLetters dict =
            match remainingLetters with
            | [] -> failwith "Will not happen"
            | x::xs ->
                let result = step x dict
                match result with
                | Some (isEndOfWord, dict) ->
                    match xs with
                    | [] -> isEndOfWord
                    | _ -> aux xs dict
                | Option.None -> false
        let searchString = str |> List.ofSeq |> List.rev |> (fun lst -> lst @ ['#'])
        aux searchString dict
        
    let print (dict:Gaddag) =
        let rec printWord (v:Gaddag) (word:Key list) =
            if v.isEndOfWord then printfn "%A" word
            Map.iter (fun k v -> printWord v (word @ [k])) v.map
        printfn "%d" dict.map.Count
        Map.iter (fun k v -> printWord v [k]) dict.map
        
    let rec nodeCount (dict:Gaddag) = // Does not account for the root, as not letter points to the root
        dict.map.Count + Map.fold (fun acc _ v -> acc + nodeCount v) 0 dict.map
        // With one word of length n and duplicate letters in the gaddag, the nodeCount will be (n+1)^2
        // The +1 comes from the '#' being added to the word
        
    let rec maxDepth (dict:Gaddag) : int =
        let rec aux (dict:Map<Key, Gaddag>) (depth:int) =
            dict |> Map.toList |> List.map (fun (_, gad) ->
                match gad.map.Count with
                | 0 -> 0
                | _ -> 1 + aux gad.map depth) |> List.max
        aux dict.map 0