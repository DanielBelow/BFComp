module BFComp.Parser

open BFComp.Lexer

type Node =
| IPtr of int | DPtr of int
| IVal of int | DVal of int
| PChar | GChar
| Loop of Node list
    
let private generateNodes tkns =
    let rec impl tkns lst =
        match tkns with
        | [] -> lst
        | Noop::xs -> impl xs lst
        | Inc_Ptr::xs -> 
            IPtr(1) :: impl xs lst
        | Dec_Ptr::xs -> 
            DPtr(1) :: impl xs lst
        | Inc_Val::xs -> 
            IVal(1) :: impl xs lst
        | Dec_Val::xs -> 
            DVal(1) :: impl xs lst
        | GetChar::xs -> 
            GChar :: impl xs lst
        | PutChar::xs -> 
            PChar :: impl xs lst
        | WhileBegin::xs -> 
            let mutable numOpen = 1
            let stmtsInLoop = xs |> List.takeWhile (fun e ->
                match e with
                | WhileBegin -> numOpen <- numOpen + 1
                | WhileEnd -> numOpen <- numOpen - 1
                | _ -> ()
                numOpen <> 0)
                
            let stmtsOutside = xs |> List.skip (List.length stmtsInLoop)                
                
            Loop(impl stmtsInLoop []) :: impl stmtsOutside lst
        | WhileEnd::xs -> impl xs lst
        
    impl tkns []

let generateAST tokens =
    let nodes = generateNodes tokens
    //printf "After parse:\n%A\n" nodes
    nodes
