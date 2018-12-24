open BFComp.Lexer
open BFComp.Parser
open BFComp.CodeGen

let private compile file = 
    parseTokens 
    >> generateAST 
    >> generateCode file
    
let private readFile (inp: string) =
    let inp = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), inp)
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
        let inputFile = argv |> Array.item 0
        inputFile |> readFile |> String.concat "" |> compile inputFile
        0