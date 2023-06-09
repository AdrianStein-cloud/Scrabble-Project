﻿module Dictionary
    type Dictionary = { 
        mutable isWord : bool;
        mutable children : Map<char, Dictionary> }

    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool
    val step : char -> Dictionary -> (bool * Dictionary) option

