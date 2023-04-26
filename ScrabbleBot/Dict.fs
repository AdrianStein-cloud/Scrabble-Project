module Dictionary
    type Dictionary = { Inner : List<string> }

    let empty (u: unit) = { Inner = List.empty }

    let insert (s: string) (dict: Dictionary) : Dictionary =
        { dict with Inner = s :: dict.Inner }

    let lookup (s: string) (dict: Dictionary) : bool =
        List.contains s dict.Inner

