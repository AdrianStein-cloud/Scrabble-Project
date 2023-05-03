module internal MoveGeneration
open ScrabbleUtil
open Parser
open MultiSet

type move = (coord * (uint32 * (char * int))) list

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

let generateStartCoords (pp : Map<coord, char>) (board : board) (coords : coord list) (step : coord -> coord) : coord list =

    let rec collect c n =
        let next = step c
        let backIsEmpty = Map.containsKey next pp |> not
        match (board.isOnBoard c, Map.containsKey c pp, backIsEmpty, n) with
        | (false, _, _, _) | (_, false, _, 0) -> Set.empty
        | (true, true, true, n) -> Set.add c (collect next n)
        | (true, false, true, n) -> Set.add c (collect next (n - 1))
        | (true, false, false, n) -> collect next (n - 1)
        | (true, true, false, n) -> collect next n
    
    let hStartCords = List.fold (fun s c -> Set.union (collect c 7) s) Set.empty coords
    Set.toList hStartCords

let generateConstraints (dict : Dictionary.Dict) (pp : Map<coord, char>) (board : board) (coords : coord list) (stepBack : coord -> coord) (stepForward : coord -> coord) : Map<coord, (char -> bool)> =
    
    let stretch (nextCoord : coord -> coord) (c : coord) =
        let rec stretchR (c : coord) =
            let next = nextCoord c
            if (board.isOnBoard next |> not) || (Map.containsKey next pp |> not) then c
            else stretchR next
        stretchR c

    let generateVConstraint (c : coord) : (char -> bool) =
        let upper = stretch stepBack c
        let lower = stretch stepForward c

        let constraintFunc (c : coord) (char : char) = 
            let pp' = Map.add c char pp
            let word = stepFold (fun s c -> s + string(Map.find c pp')) "" upper stepForward (fun c -> (stepBack c) = lower |> not)
            let valid = Dictionary.lookup word dict
            valid

        if upper = lower then (fun _ -> true)
        else constraintFunc c

    let constraints = List.fold (fun s c -> Map.add c (generateVConstraint c) s) Map.empty coords
    constraints

let generateMove (dict : Dictionary.Dict) (pp : Map<coord, char>) (start : coord) (constraints : (coord -> char -> bool)) (hand : MultiSet<(uint32 * (char * int) list)>) (board : board) (coords : coord list) (step : coord -> coord) : move list = 

    let rec folder (c : coord) (touchedValidStartPoint : bool) (move : move) (dict : Dictionary.Dict) (hand : MultiSet<(uint32 * (char * int) list)>) (moves : move list) (piece : (uint32 * (char * int) list)) (n : uint32) : move list = 
        let charPointPairList = snd piece
        let hand' = MultiSet.removeSingle piece hand
        let testChar (charPointPair : (char * int)) : move list =
            let char = fst charPointPair
            let move' = (c, (fst piece, (charPointPair))) :: move
            let next = step c
            let backIsEmpty = Map.containsKey next pp |> not
            match (Dictionary.step char dict, constraints c char, touchedValidStartPoint && backIsEmpty) with
            | (Some (true, dict'), true, true) -> move' :: foldOverPlaceablePieces next touchedValidStartPoint move' dict' hand'
            | (Some (_, dict'), true, _) -> foldOverPlaceablePieces next touchedValidStartPoint move' dict' hand'
            | _ -> List.empty

        moves @ List.fold (fun s (charPointPair) -> s @ testChar charPointPair) List.empty charPointPairList

    and foldOverPlaceablePieces (c : coord) (touchedValidStartPoint : bool) (move : move) (dict : Dictionary.Dict) (hand : MultiSet<(uint32 * (char * int) list)>) = 
        match (Map.containsKey c pp, board.isOnBoard c) with
        | (false, true) -> 
            let touchedValidStartPoint' = touchedValidStartPoint || List.contains c coords
            MultiSet.fold (folder c touchedValidStartPoint' move dict hand) List.empty hand
        | (true, true) -> 
            let char = Map.find c pp
            let next = step c
            let backIsEmpty = Map.containsKey next pp |> not
            match (Dictionary.step char dict, backIsEmpty, touchedValidStartPoint) with
            | (Some (true, dict'), true, true) -> move :: foldOverPlaceablePieces next touchedValidStartPoint move dict' hand
            | (Some (_, dict'), _, _) -> foldOverPlaceablePieces next touchedValidStartPoint move dict' hand
            | _ -> List.empty
        | (_, false) -> List.Empty

    let moves = foldOverPlaceablePieces start false List.empty dict hand
    moves

let evalMove (m : move) (b : board) : int = 
    List.fold (fun acc (mov : (coord * (uint32 * (char * int)))) -> acc + (mov |> snd |> snd |> snd)) 0 m

let calculateMoveValues (moves : move list) (board : board) : ((int * move) list) = 
    moves 
        |> List.map (fun move -> (evalMove move board, move))
        |> List.sortByDescending (fun (eval, _) -> eval)

let generateMoves (dict : Dictionary.Dict) (pp : Map<coord, char>) (board : board) (hand : MultiSet<(uint32 * (char * int) list)>) : ((int * move) list) = 
    let coords = generateCoords pp board
    //DebugPrint.forcePrint (sprintf "Coords: %A\n" coords)
    let hStartCoords = generateStartCoords pp board coords left
    let vStartCoords = generateStartCoords pp board coords up
    //DebugPrint.forcePrint (sprintf "hlines: %A\n" hStartCoords)
    let vConstraints = checkConstraint (generateConstraints dict pp board coords up down)
    let hConstraints = checkConstraint (generateConstraints dict pp board coords left right)
    let hMoves = List.fold (fun ms c -> ms @ (generateMove dict pp c vConstraints hand board coords right)) List.empty hStartCoords
    let vMoves = List.fold (fun ms c -> ms @ (generateMove dict pp c hConstraints hand board coords down)) List.empty vStartCoords
    let moves = hMoves @ vMoves

    let moveValues = calculateMoveValues moves board
    moveValues