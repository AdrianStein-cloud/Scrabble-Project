namespace ScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        turnId        : uint32
        placedPieces  : Map<coord, char>
        players       : List<uint32>
        piecesLeft    : uint32
    }

    let mkState b d pn h pt np = {board = b; dict = d;  playerNumber = pn; hand = h; turnId = pt - 1u; placedPieces = Map.empty; players = [1u..np]; piecesLeft = 100u - ((MultiSet.size h) * np)}

module Scrabble =
    open System.Threading

    let playGame cstream (pieces : Map<uint32, tile>) (st : State.state) =

        let rec aux (st : State.state) =
            let mutable swappedPieces : uint32 list = []
            if (st.playerNumber = st.players[(int)st.turnId]) then

                let gHand = MultiSet.fold (fun s pid n -> MultiSet.add (pid, (Map.find pid pieces |> Set.map (fun t -> t) |> Set.toList)) n s) MultiSet.empty st.hand
                let moves = MoveGeneration.generateMoves st.dict st.placedPieces st.board gHand
                match moves.Length with
                | 0 -> 
                    if st.piecesLeft > 0u then
                        swappedPieces <- MultiSet.fold (fun s1 pid n -> s1 @ List.fold (fun s2 _ -> if s1.Length + s2.Length < int st.piecesLeft then pid :: s2 else s2) [] [1..int n]) [] st.hand
                        send cstream (SMChange swappedPieces)
                    else send cstream SMPass
                | _ -> 
                    let move = (List.head moves) |> snd
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (st.playerNumber) move) 
                    send cstream (SMPlay move)
            


            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (st.playerNumber) msg)

            let updatePlacedPieces (ms : (coord * (uint32 * (char * int)))list) (pp : Map<coord, char>) =
                List.fold (fun x y -> Map.add (fst y) (y |> snd |> snd |> fst) x) pp ms

            let updatePlayerTurn (st : State.state) = 
                let newTurn = st.turnId + 1u
                if ((int)newTurn >= st.players.Length) then 0u
                else newTurn

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->

                //Update board
                let newPlacedPieces = updatePlacedPieces ms st.placedPieces

                //Update hand
                let remove (m : MultiSet.MultiSet<uint32>) (tilePlacementMove : coord * (uint32 * (char * int))) : MultiSet.MultiSet<uint32> = 
                    let pieceId = tilePlacementMove |> snd |> fst
                    MultiSet.removeSingle pieceId m

                let add m p = MultiSet.add (fst p) (snd p) m

                let mutable newHand = st.hand
                newHand <- List.fold remove newHand ms
                newHand <- List.fold add newHand newPieces
                

                let st' = {st with hand = newHand; placedPieces = newPlacedPieces; turnId = updatePlayerTurn st; piecesLeft = st.piecesLeft - (uint32)(List.length ms)}
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->

                //Update board
                let newPlacedPieces = updatePlacedPieces ms st.placedPieces

                let st' = {st with placedPieces = newPlacedPieces; turnId = updatePlayerTurn st; piecesLeft = st.piecesLeft - (uint32)(List.length ms)}
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->

                let st' = {st with turnId = updatePlayerTurn st}
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMChangeSuccess pieces) -> 
                
                let mutable newHand = st.hand
                newHand <- List.fold (fun m p -> MultiSet.removeSingle p m) newHand swappedPieces
                newHand <- List.fold (fun m p -> (MultiSet.add (fst p) (snd p) m)) newHand pieces
                let st' = {st with turnId = updatePlayerTurn st; hand = newHand}
                aux st'
            | RCM (CMChange (playerId, numOfTiles)) -> 
                let st' = {st with turnId = updatePlayerTurn st}
                aux st'
            | RCM (CMForfeit playerId) -> 
                let st' = {st with turnId = updatePlayerTurn st; players = List.removeAt ((int)playerId) st.players; piecesLeft = st.piecesLeft + (MultiSet.size st.hand)}
                aux st'
            | RCM (CMPassed playerId) | RCM (CMTimeout playerId) -> 
                let st' = {st with turnId = updatePlayerTurn st}
                aux st'
            | RGPE err -> debugPrint (sprintf "Gameplay Error:\n%A\n" err); aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers)
        
        