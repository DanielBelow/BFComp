module BFComp.CombineInst

open BFComp.Parser

let rec combineInsts ast =
    let rec impl inp acc =
        match inp with
        | [] -> acc
        | IVal(n)::xs ->
            let incVals = 
                inp 
                |> List.takeWhile(function | IVal(n) -> true | _ -> false) 
                |> List.map (fun e -> 
                    let IVal(n) = e 
                    n
                ) 
            let cnt = incVals |> List.sum
            let toSkip = incVals |> List.length
            IVal(cnt) :: impl (List.skip (toSkip - 1) xs) acc                
        | DVal(n)::xs ->
            let decVals = 
                inp 
                |> List.takeWhile(function | DVal(n) -> true | _ -> false) 
                |> List.map (fun e -> 
                    let DVal(n) = e 
                    n
                ) 
            let cnt = decVals |> List.sum
            let toSkip = decVals |> List.length
            DVal(cnt) :: impl (List.skip (toSkip - 1) xs) acc                
        | IPtr(n)::xs ->
            let incPtrs = 
                inp 
                |> List.takeWhile(function | IPtr(n) -> true | _ -> false) 
                |> List.map (fun e -> 
                    let IPtr(n) = e 
                    n
                ) 
            let cnt = incPtrs |> List.sum
            let toSkip = incPtrs |> List.length
            IPtr(cnt) :: impl (List.skip (toSkip - 1) xs) acc                
        | DPtr(n)::xs ->
            let decPtrs = 
                inp 
                |> List.takeWhile(function | DPtr(n) -> true | _ -> false) 
                |> List.map (fun e -> 
                    let DPtr(n) = e 
                    n
                ) 
            let cnt = decPtrs |> List.sum
            let toSkip = decPtrs |> List.length
            DPtr(cnt) :: impl (List.skip (toSkip - 1) xs) acc                
        | Loop(stmts)::xs ->
            Loop(combineInsts stmts) :: impl xs acc
        | x::xs -> x :: impl xs acc
       
    impl ast []