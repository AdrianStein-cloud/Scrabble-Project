module Dictionary
    type Dictionary = { 
        mutable isWord : bool;
        mutable children : Map<char, Dictionary> }

    let empty (u: unit) = { isWord = false; children = Map.empty }

    let insert (s: string) (dict: Dictionary) : Dictionary = 
        let cArray = Seq.toList(s)
        let rec ins (c : char list) (d : Dictionary) : unit =
            match c with
            | x::[] -> if Map.containsKey x d.children then (Map.find x d.children).isWord <- true
                       else 
                           let child = empty ()
                           child.isWord <- true
                           d.children <- Map.add x child d.children
            | x::xs -> if Map.containsKey x d.children then ins xs (Map.find x d.children)
                       else 
                           let child = empty ()
                           d.children <- Map.add x child d.children
                           ins xs child
            | _ -> ()
        ins cArray dict
        dict

    let lookup (s : string) (dict : Dictionary) : bool =
        let cArray = Seq.toList(s)
        let rec look (c : char list) (d : Dictionary) : bool =
            match c with
            | x::xs -> if Map.containsKey x d.children then look xs (Map.find x d.children) else false
            | [] -> d.isWord

        look cArray dict

    let step (c : char) (d : Dictionary) : (bool * Dictionary) option =
        if Map.containsKey c d.children then 
            let child = Map.find c d.children
            Some (child.isWord, child)
        else None
            
    
