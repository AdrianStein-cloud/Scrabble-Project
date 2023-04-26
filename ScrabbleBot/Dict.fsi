module internal Dictionary
    type internal Dictionary = { Inner : List<string> }

    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool

