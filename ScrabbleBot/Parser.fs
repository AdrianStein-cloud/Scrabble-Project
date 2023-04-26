// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = whitespaceChar |> many
    let spaces1        = whitespaceChar |> many1 <?> "space1"

    let (.>*>.) a b = (a .>> spaces .>>. b)
    let (.>*>) a b = (a .>> spaces .>> b)
    let (>*>.) a b  = (a .>> spaces >>. b)

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' 

    let pid = ((pletter <|> pchar '_') .>>. ((palphanumeric <|> pchar '_') |> many)) |>> auxiliary

    
    let unop a b = a >*>. b
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let ParParse = parenthesise TermParse
    let NParse   = pint32 |>> N <?> "Int"
    let PVParse =  pstring "pointValue" >*>. parenthesise TermParse |>> PV <?> "PV"
    let VParse =  pid |>> V <?> "Var"
    let NegParse = (pchar '-') >*>. TermParse |>> (fun x -> Mul(N -1, x)) <?> "Neg"
    let CharToInt = pCharToInt >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToInt;NegParse; PVParse; VParse; NParse; ParParse]


    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    let CVParse = pCharValue >*>. parenthesise TermParse |>> CV <?> "CV"
    let IntToCharParse = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    let UpperParse = pToUpper >*>. parenthesise CharParse |>> ToUpper <?> "ToUpper"
    let LowerParse = pToLower >*>. parenthesise CharParse |>> ToLower <?> "ToLower"
    do cref := choice [CVParse; IntToCharParse; UpperParse; LowerParse; CParse]

    let AexpParse = TermParse 

    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
