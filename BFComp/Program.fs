open BFComp.Lexer
open BFComp.Parser
open BFComp.CodeGen

open System

let hello_world = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.+++."

let private compile = 
    parseTokens 
    >> generateAST 
    >> generateCode

[<EntryPoint>]
let main argv =
    compile hello_world
    0