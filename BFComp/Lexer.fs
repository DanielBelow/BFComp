module BFComp.Lexer

type Token =
    | Inc_Ptr | Dec_Ptr
    | Inc_Val | Dec_Val
    | PutChar | GetChar
    | WhileBegin | WhileEnd
    | Noop
    
let private toToken = function 
    | '>' -> Inc_Ptr
    | '<' -> Dec_Ptr
    | '+' -> Inc_Val
    | '-' -> Dec_Val
    | '.' -> PutChar
    | ',' -> GetChar
    | '[' -> WhileBegin
    | ']' -> WhileEnd
    | _ -> Noop
    
let parseTokens inp = Seq.map (toToken) inp |> Seq.toList