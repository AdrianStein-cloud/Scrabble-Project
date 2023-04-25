module MultiSet

    type MultiSet<'a when 'a : comparison> =
        { Inner : Map<'a, uint32> }

    let empty = { Inner = Map.empty }

    let isEmpty<'a when 'a : comparison> (multiset: MultiSet<'a>) : bool =
        match (multiset.Inner |> Map.fold (fun acc _ count -> acc + count) 0u) with
            | 0u -> true
            | _ -> false

    let size<'a when 'a : comparison> (multiset: MultiSet<'a>) : uint32 =
        multiset.Inner |> Map.fold (fun acc _ count -> acc + count) 0u

    let contains<'a when 'a : comparison> (item: 'a) (multiset: MultiSet<'a>) : bool =
        match Map.tryFind item multiset.Inner with
            | Some _ -> true
            | None -> false

    let numItems<'a when 'a : comparison> (item: 'a) (multiset: MultiSet<'a>) : uint32 =
        match Map.tryFind item multiset.Inner with
            | Some num -> num
            | None -> 0u

    let add<'a when 'a : comparison> (item: 'a) (count: uint32) (multiset: MultiSet<'a>) : MultiSet<'a> =
        let newCount = 
            match Map.tryFind item multiset.Inner with
            | Some c -> c + count
            | None -> count
        { multiset with Inner = Map.add item newCount multiset.Inner }

    let addSingle<'a when 'a : comparison> (item: 'a) (multiset: MultiSet<'a>) : MultiSet<'a> =
        { multiset with Inner = Map.add item 1u multiset.Inner }

    let remove<'a when 'a : comparison> (item: 'a) (count: uint32) (multiset: MultiSet<'a>) : MultiSet<'a> =
        match Map.tryFind item multiset.Inner with
        | Some n when n >= count ->
            let newCount = n - count
            if newCount = 0u then { multiset with Inner = Map.remove item multiset.Inner }
            else { multiset with Inner = Map.add item newCount multiset.Inner }
        | Some count -> { multiset with Inner = Map.add item 0u multiset.Inner }
        | None -> multiset

    let removeSingle<'a when 'a : comparison> (item: 'a) (multiset: MultiSet<'a>) : MultiSet<'a> =
        match Map.tryFind item multiset.Inner with
        | Some count when count > 1u ->
            { multiset with Inner = Map.add item (count - 1u) multiset.Inner }
        | Some _ ->
            { multiset with Inner = Map.remove item multiset.Inner }
        | None -> multiset

    let fold<'a, 'b when 'b : comparison> (f: 'a -> 'b -> uint32 -> 'a) (acc: 'a) (multiset: MultiSet<'b>) : 'a = Map.fold f acc multiset.Inner

    let foldBack<'a, 'b when 'a : comparison> (f: 'a -> uint32-> 'b -> 'b) (multiset: MultiSet<'a>) (acc: 'b) : 'b = Map.foldBack f multiset.Inner acc
        
