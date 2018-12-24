module BFComp.Parser

open BFComp.Lexer

type Node =
| IPtr of int | DPtr of int
| IVal of int | DVal of int
| PChar | GChar
| Loop of Node list

let private nodesInLoop lp =
     let mutable numOpen = 1
     lp |> List.takeWhile (fun e ->
         match e with
         | WhileBegin -> numOpen <- numOpen + 1
         | WhileEnd -> numOpen <- numOpen - 1
         | _ -> ()
         numOpen <> 0)
    
let private generateNodes tkns =
    let rec impl tkns lst =
        match tkns with
        | [] -> lst
        | Inc_Ptr::xs -> IPtr(1) :: impl xs lst
        | Dec_Ptr::xs -> DPtr(1) :: impl xs lst
        | Inc_Val::xs -> IVal(1) :: impl xs lst
        | Dec_Val::xs -> DVal(1) :: impl xs lst
        | GetChar::xs -> GChar :: impl xs lst
        | PutChar::xs -> PChar :: impl xs lst
        | WhileBegin::xs -> 
            let stmtsInLoop = nodesInLoop xs
            let afterLoop = xs |> List.skip (List.length stmtsInLoop)                
            Loop(impl stmtsInLoop []) :: impl afterLoop lst
        | WhileEnd::xs | Noop::xs -> impl xs lst
        
    impl tkns []

let generateAST tokens = generateNodes tokens