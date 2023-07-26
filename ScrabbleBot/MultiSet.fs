module internal MultiSet
    type MultiSet<'a when 'a : comparison> = { map : Map<'a, uint32>; size : uint32}
    let empty : MultiSet<'a> = { map = Map.empty; size = 0u }
    let isEmpty (multiSet:MultiSet<'a>) : bool = multiSet.size = 0u
    let size (multiSet:MultiSet<'a>) =
        multiSet.size
    let contains (a:'a) (multiSet:MultiSet<'a>) : bool =
        Map.containsKey a multiSet.map
    let numItems (a:'a) (multiSet:MultiSet<'a>) : uint32 =
        if contains a multiSet then multiSet.map.[a]
        else 0u
    let add (a:'a) (amount:uint32) (multiSet:MultiSet<'a>) : MultiSet<'a> =
        if contains a multiSet then
            { map = Map.add a (multiSet.map.[a] + amount) multiSet.map; size = multiSet.size + amount }
        else
            { map = Map.add a amount multiSet.map; size = multiSet.size + amount }
    let addSingle (a:'a) (multiSet:MultiSet<'a>) : MultiSet<'a> =
        add a 1u multiSet
    
    let remove (a:'a) (amount:uint32) (multiSet:MultiSet<'a>) : MultiSet<'a> =
        if contains a multiSet then
            match multiSet.map.[a] with
            | count when count <= amount -> { map = Map.remove a multiSet.map; size = multiSet.size - count } 
            | count -> { map = Map.add a (count-amount) multiSet.map; size = multiSet.size - amount }
        else multiSet
        
    let removeSingle (a:'a) (multiSet:MultiSet<'a>) : MultiSet<'a> =
        remove a 1u multiSet
        
    let fold (f:'a -> 'b -> uint32 -> 'a) (acc:'a) (multiSet:MultiSet<'b>) : 'a =
        Map.fold f acc multiSet.map
    
    let foldBack f (multiSet:MultiSet<'a>) (acc:'b) : 'b =
        Map.foldBack f multiSet.map acc
    
    let ofList (list:'a list) : MultiSet<'a> =
        let newList =
            list
            |> List.groupBy id
            |> List.map (fun (element, list) -> (element, uint32 list.Length))
            |> Map.ofList
        { map = newList; size = uint32 list.Length }
        
    let toList (multiSet:MultiSet<'a>) : 'a list =
        Map.toList multiSet.map
        |> List.map (fun (element, amount) -> List.replicate (int amount) element)
        |> List.concat
        
    let map (f:'a -> 'b) (multiSet:MultiSet<'a>) : MultiSet<'b> =
        fold (fun acc key value ->
            add (f key) value acc
            ) empty multiSet
    
    let union (multiSet1:MultiSet<'a>) (multiSet2:MultiSet<'a>) : MultiSet<'a> =
        fold (fun acc key value ->
            let highestCount = max value acc.map.[key]
            remove key acc.map.[key] acc
            |> add key highestCount) multiSet1 multiSet2
        
    let sum (multiSet1:MultiSet<'a>) (multiSet2:MultiSet<'a>) : MultiSet<'a> =
        fold (fun acc key value -> add key value acc) multiSet1 multiSet2
    
    let subtract (multiSet1:MultiSet<'a>) (multiSet2:MultiSet<'a>) : MultiSet<'a> =
        fold (fun acc key value -> remove key value acc) multiSet1 multiSet2
    
    let intersection (multiSet1:MultiSet<'a>) (multiSet2:MultiSet<'a>) : MultiSet<'a> =
        fold (fun acc key value ->
            if contains key multiSet2 then
                let leastCount = min value (numItems key multiSet2)
                if (leastCount <> 0u) then add key leastCount acc
                else acc
            else
                acc
            ) empty multiSet1

    // printfn "%d" (numItems "There" (add "Hello" 4u (add "There" 5u empty)))

    // let hellos = (add "His" 1u empty)
    // let his = add "Hello" 2u (add "His" 0u empty)
    // printfn "%A" ((sum hellos his).map)
    // printfn "%A" ((sum hellos his).size)