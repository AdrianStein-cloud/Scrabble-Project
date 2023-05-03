module internal MoveGeneration
open ScrabbleUtil
open Parser
open MultiSet

type move = (coord * (uint32 * char)) list

let right (c : coord) : coord = (fst c + 1, snd c)
let left (c : coord) : coord = (fst c - 1, snd c)
let up (c : coord) : coord = (fst c, snd c - 1)
let down (c : coord) : coord = (fst c, snd c + 1)

let stepFold folder (state : 'State) (start : coord) step conditon =
    let rec recFold state cur =
        if conditon cur then recFold (folder state cur) (step cur)
        else state
    recFold state start

let checkConstraint (constraints : Map<coord, (char -> bool)>) (coord : coord) (c : char) : bool = 
    match (Map.tryFind coord constraints) with
    | Some condition -> condition c
    | None -> true

let getValidNeighbours (board : board) (pp : Map<coord, char>) (c : coord) : coord list = 
    let neighbours : coord list = [(fst c, snd c + 1); (fst c, snd c - 1); (fst c + 1, snd c); (fst c - 1, snd c)]
    let validNeighbours = List.filter (fun c -> (Map.containsKey c pp |> not) && board.isOnBoard c) neighbours
    validNeighbours

let generateCoords (pp : Map<coord, char>) (board : board) : coord list = 
    if Map.isEmpty pp then [(0,0)]
    else 
        let coords = Map.fold (fun l k _ -> l @ (getValidNeighbours board pp k)) List.empty pp
        let distinctCoords = Seq.distinct coords |> Seq.toList
        distinctCoords

let generateHLines (pp : Map<coord, char>) (board : board) (coords : coord list) : (coord * coord) list = 
    let f (c : coord) : (coord * coord) =

        let stretch (nextCoord : coord -> coord) (c : coord) (n : int) =
            let rec stretchR (c : coord) (n : int) =
                let next = nextCoord c
                if board.isOnBoard next |> not then c
                elif Map.containsKey next pp then stretchR next n
                elif n = 0 then c
                else stretchR next (n - 1)
            stretchR c n

        let stretchRight = stretch right
        let stretchLeft = stretch left
        
        (stretchLeft c 0, stretchRight c 3)
    let hLines = List.map f coords
    hLines

let generateVLines (pp : Map<coord, char>) (board : board) (coords : coord list) : (coord * coord) list = failwith "Not implemented"

let generateHConstraints (dict : Dictionary.Dict) (pp : Map<coord, char>) (board : board) (coords : coord list) : Map<coord, (char -> bool)> = failwith "Not implemented"

let generateVConstraints (dict : Dictionary.Dict) (pp : Map<coord, char>) (board : board) (coords : coord list) : Map<coord, (char -> bool)> =
    
    let stretch (nextCoord : coord -> coord) (c : coord) =
        let rec stretchR (c : coord) =
            let next = nextCoord c
            if (board.isOnBoard next |> not) || (Map.containsKey next pp |> not) then c
            else stretchR next
        stretchR c
    
    let stretchUp = stretch up
    let stretchDown = stretch down

    let generateVConstraint (c : coord) : (char -> bool) =
        let upper = stretchUp c
        let lower = stretchDown c

        let constraintFunc (c : coord) (char : char) = 
            let pp' = Map.add c char pp
            let word = stepFold (fun s c -> s + string(Map.find c pp')) "" upper down (fun c -> (up c) = lower |> not)
            let valid = Dictionary.lookup word dict
            valid

        if upper = lower then (fun _ -> true)
        else constraintFunc c

    let constraints = List.fold (fun s c -> Map.add c (generateVConstraint c) s) Map.empty coords
    constraints

let generateHMove (dict : Dictionary.Dict) (pp : Map<coord, char>) (line : coord * coord) (constraints : (coord -> char -> bool)) (hand : MultiSet<(uint32 * char list)>) : move list = 
    
    let rec folder (c : coord) (move : move) (dict : Dictionary.Dict) (hand : MultiSet<(uint32 * char list)>) (moves : move list) (piece : (uint32 * char list)) (_ : uint32) : move list = 
        
        let char, hand', move' = 
            if Map.containsKey c pp then Map.find c pp, hand, move
            else 
                let char = List.head (snd piece)
                char, MultiSet.removeSingle piece hand, (c, (fst piece, char)) :: move //Wildcards always becomes A

        match (Dictionary.step char dict, constraints c char) with
        | (Some (isWord, newDict), true) -> 
            let accumulatedMoves = moves @ multiFold (right c) move' newDict hand'
            if isWord then move' :: accumulatedMoves
            else accumulatedMoves
        | _ -> moves

    and multiFold (c : coord) (move : move) (dict : Dictionary.Dict) (hand : MultiSet<(uint32 * char list)>) = 
        MultiSet.fold (folder c move dict hand) List.empty hand 

    let moves = multiFold (fst line) List.empty dict hand
    moves

let generateVMove (dict : Dictionary.Dict) (pp : Map<coord, char>) (line : coord * coord) (constraints : (coord -> char -> bool)) (hand : MultiSet<(uint32 * char list)>) : move list = failwith "Not implemented"


let generateMoves (dict : Dictionary.Dict) (pp : Map<coord, char>) (board : board) (hand : MultiSet<(uint32 * char list)>) : move list = 
    let coords = generateCoords pp board
    DebugPrint.forcePrint (sprintf "Coords: %A\n" coords)
    let hLines = generateHLines pp board coords
    DebugPrint.forcePrint (sprintf "hlines: %A\n" hLines)
    let vConstraints = checkConstraint (generateVConstraints dict pp board coords)
    let hMoves = List.fold (fun ms l -> ms @ (generateHMove dict pp l vConstraints hand)) List.empty hLines
    hMoves