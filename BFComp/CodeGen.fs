module BFComp.CodeGen

open BFComp.Parser
open BFComp.CombineInst

open LLVMSharp

let private generateModule = 
    let m = LLVM.ModuleCreateWithName("main.bf")
    LLVM.SetTarget(m, "x86_64-pc-linux-gnu")
    m
    
let private addAttr m f at =
    let attrKind = LLVM.GetEnumAttributeKindForName(at, size_t (nativeint (String.length at)))
    let attr = LLVM.CreateEnumAttribute(LLVM.GetModuleContext(m), attrKind, uint64 0)
    LLVM.AddAttributeAtIndex(f, LLVMAttributeIndex.LLVMAttributeFunctionIndex, attr)
        
let private generateFunction m =
    let funcType = LLVM.FunctionType(LLVMTypeRef.Int32Type(), [||], false)
    let f = LLVM.AddFunction(m, "main", funcType)
    ["noinline"; "nounwind"; "uwtable"] |> List.iter (addAttr m f)
    f
    
let private constInt v intTy = LLVM.ConstInt(intTy, uint64 v, LLVMBool(0))
let private constI8 n = constInt n (LLVM.Int8Type())
let private constI32 n = constInt n (LLVM.Int32Type())
let private constZeroI8 = constI8 0
let private constOneI8 = constI8 1
let private constZeroI32 = constI32 0
let private constOneI32 = constI32 1
    
let private loadCurrentStackValue builder stack ptr =
    let ptrLd = LLVM.BuildLoad(builder, ptr, "")
    let gep = LLVM.BuildInBoundsGEP(builder, stack, [|constZeroI32; ptrLd|], "")            
    (gep, LLVM.BuildLoad(builder, gep, ""))
    
let private addToPtr builder ptr n =    
    let ptrLd = LLVM.BuildLoad(builder, ptr, "")
    let nxt = LLVM.BuildAdd(builder, ptrLd, constI32 n, "")
    LLVM.BuildStore(builder, nxt, ptr)
    
let private addToValue builder stack ptr n =
    let (gep, oldValue) = loadCurrentStackValue builder stack ptr
    let newValue = LLVM.BuildAdd(builder, oldValue, constI8 n, "")
    LLVM.BuildStore(builder, newValue, gep)
    
let private putChar m builder stack ptr =     
    let putcharFunc = LLVM.GetNamedFunction(m, "putchar")
    
    let (_, elem) = loadCurrentStackValue builder stack ptr
    let toPrint = LLVM.BuildZExt(builder, elem, LLVM.Int32Type(), "")
    LLVM.BuildCall(builder, putcharFunc, [| toPrint |], "")
    
let private getChar m builder stack ptr =
    let gc = LLVM.GetNamedFunction(m, "getchar")
    
    let (gep, elem) = loadCurrentStackValue builder stack ptr
    let chr = LLVM.BuildCall(builder, gc, [||], "")
    LLVM.BuildStore(builder, chr, gep)
        
let rec private buildLoop m f builder stack ptr stmts =
    let loopHdr = LLVM.AppendBasicBlock(f, "hdr")
    let loopBody = LLVM.AppendBasicBlock(f, "loop")
    let next = LLVM.AppendBasicBlock(f, "")
    
    LLVM.BuildBr(builder, loopHdr) |> ignore
    LLVM.PositionBuilderAtEnd(builder, loopHdr)
    
    let (_, elem) = loadCurrentStackValue builder stack ptr
    let cmp = LLVM.BuildICmp(builder, LLVMIntPredicate.LLVMIntNE, constZeroI8, elem, "")
    LLVM.BuildCondBr(builder, cmp, loopBody, next) |> ignore
    
    LLVM.PositionBuilderAtEnd(builder, loopBody)
    generateCodeImpl m f builder stack ptr stmts    
    
    LLVM.BuildBr(builder, loopHdr) |> ignore
    LLVM.PositionBuilderAtEnd(builder, next)
    
and private generateCodeImpl m f builder stack ptr = 
    List.iter (function
        | IPtr(n) -> addToPtr builder ptr n |> ignore
        | DPtr(n) -> addToPtr builder ptr -n |> ignore
        | IVal(n) -> addToValue builder stack ptr n |> ignore
        | DVal(n) -> addToValue builder stack ptr -n |> ignore
        | PChar -> putChar m builder stack ptr |> ignore
        | GChar -> getChar m builder stack ptr |> ignore
        | Loop(stmts) -> buildLoop m f builder stack ptr stmts
    )
    
let private initializeStack builder =
    let stackSize = 128
    let stack = LLVM.BuildAlloca(builder, LLVM.ArrayType(LLVM.Int8Type(), uint32 stackSize), "stack")
    let zeroArray = LLVM.ConstArray(LLVM.Int8Type(), Array.init stackSize (fun _ -> constZeroI8))
    LLVM.BuildStore(builder, zeroArray, stack) |> ignore
    stack
    
let private initializePtr builder =
    let ptr = LLVM.BuildAlloca(builder, LLVM.Int32Type(), "ptr")
    LLVM.BuildStore(builder, LLVM.ConstInt(LLVM.Int32Type(), uint64 0, LLVMBool(0)), ptr) |> ignore
    ptr
    
let private writeToFile (f: string) m =    
    let subs = f.LastIndexOf(".")
    let outputFile = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), f.Substring(0, subs) + ".bc")
    LLVM.WriteBitcodeToFile(m, outputFile) |> ignore
    
    
    
let generateCode file ast =
    // combine consecutive + / - / > / < into a single operation
    let ast = combineInsts ast
    
    let m = generateModule
    let f = generateFunction m
    
    LLVM.AddFunction(m, "putchar", LLVM.FunctionType(LLVM.Int32Type(), [| LLVM.Int32Type() |], false)) |> ignore
    LLVM.AddFunction(m, "getchar", LLVM.FunctionType(LLVM.Int32Type(), [||], false)) |> ignore
    
    let entryBB = LLVM.AppendBasicBlock(f, "entry")
    let builder = LLVM.CreateBuilder()
    LLVM.PositionBuilderAtEnd(builder, entryBB)
    
    let stack = initializeStack builder
    let ptr = initializePtr builder
    
    generateCodeImpl m f builder stack ptr ast
    LLVM.BuildRet(builder, constZeroI32) |> ignore
    
    LLVM.VerifyModule(m, LLVMVerifierFailureAction.LLVMAbortProcessAction) |> ignore
    
    writeToFile file m
    
    LLVM.DisposeBuilder(builder)
    LLVM.DisposeModule(m)