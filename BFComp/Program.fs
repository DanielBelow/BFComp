open BFComp.Lexer
open BFComp.Parser
open BFComp.CodeGen

let private compile = 
    parseTokens 
    >> generateAST 
    >> generateCode
    
let private readFile (inp: string) = 
    printf "Reading %s\n" inp
    seq {
    use sr = new System.IO.StreamReader(inp)
    while not sr.EndOfStream do
        yield sr.ReadLine()
}
        
[<EntryPoint>]
let main argv =
    if Array.length argv <> 1 then
        printf "Invalid number of arguments."
        -1
    else
        argv |> Array.item 0 |> readFile |> String.concat "" |> compile
        0