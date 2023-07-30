module Dictionary
    type Dictionary =
        { map : Map<char, Dictionary>; isEndOfWord : bool }
    let empty () = { map = Map.empty; isEndOfWord = false }        
    let insert (str:string) (dict:Dictionary) : Dictionary =
        let rec insertCharList charList dict = // avoid converting back and fourth between charList and string
            match charList with
            | [] -> { map = dict.map; isEndOfWord = true }
            | x::xs ->
                match x with
                | x when Map.containsKey x dict.map ->
                    let updatedChildDict = insertCharList xs dict.map.[x]
                    let updatedMap = Map.add x updatedChildDict dict.map
                    { map = updatedMap; isEndOfWord = dict.isEndOfWord }
                | _ ->
                    let updatedChildDict = insertCharList xs (empty ())
                    let updatedMap = Map.add x updatedChildDict dict.map
                    { map = updatedMap; isEndOfWord = dict.isEndOfWord }
        insertCharList (List.ofSeq str) dict
                
    let lookup (str:string) (dict:Dictionary) : bool =
        let rec lookupCharList charList dict = 
            match charList with
            | [] -> dict.isEndOfWord
            | x::xs ->
                if Map.containsKey x dict.map then lookupCharList xs dict.map.[x]
                else false
        lookupCharList (List.ofSeq str) dict

    let rec step (c:char) (dict:Dictionary) : (bool * Dictionary) option =
        if Map.containsKey c dict.map then
            let dict2 = dict.map.[c]
            Some (dict2.isEndOfWord, dict2)
        else None
        
    let reverse (dict:Dictionary) : (bool * Dictionary) option =
        failwith "not implemented"