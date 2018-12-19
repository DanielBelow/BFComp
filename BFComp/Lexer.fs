module BFComp.Lexer

type Token =
    | Inc_Ptr | Dec_Ptr
    | Inc_Val | Dec_Val
    | PutChar | GetChar
    | WhileBegin | WhileEnd
    | Noop
    
    
let private getToken lst tok =
    (match tok with
    | '>' -> Inc_Ptr
    | '<' -> Dec_Ptr
    | '+' -> Inc_Val
    | '-' -> Dec_Val
    | '.' -> PutChar
    | ',' -> GetChar
    | '[' -> WhileBegin
    | ']' -> WhileEnd
    | _ -> Noop) :: lst
    
let parseTokens inp = 
    let res = inp |> Seq.fold (getToken) [] |> Seq.rev |> Seq.toList
    // printf "Lexed: %A\n" res
    res