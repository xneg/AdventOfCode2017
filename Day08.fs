module Day08

// problem page
// http://adventofcode.com/2017/day/8

open System.Collections.Generic
open System.IO

let problemFileName = @"Data\08.txt"

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line.Split([|'\t';' '|])
    };;

let lines = problemFileName |> processFile

type Statement =
    | IfStmt of Expression * Statement
    | Inc of Expression * Expression
    | Dec of Expression * Expression
and Expression =
    | Register of string
    | Integer of int
    | Eq of Expression * Expression
    | NotEq of Expression * Expression
    | Lt of Expression * Expression
    | Gt of Expression * Expression
    | Lte of Expression * Expression
    | Gte of Expression * Expression

let toStatement (line : string[]) =
    let modifiedRegister = Register (line.[0])
    let conditionalRegister = Register (line.[4])
    let arg1 = Integer(int(line.[2]))
    let arg2 = Integer(int(line.[6]))
    let changeStatement = match line.[1] with
        | "inc" -> Inc (modifiedRegister, arg1)
        | "dec" -> Dec (modifiedRegister, arg1)   
    let condExpr = match line.[5] with
        | "==" -> Eq (conditionalRegister, arg2)
        | "!=" -> NotEq (conditionalRegister, arg2)
        | ">" -> Gt (conditionalRegister, arg2)
        | "<" -> Lt (conditionalRegister, arg2)
        | ">=" -> Gte (conditionalRegister, arg2)
        | "<=" -> Lte (conditionalRegister, arg2)
    IfStmt(condExpr, changeStatement)    

let statements = lines |> Seq.map (toStatement)

let registers = Dictionary<string, int>()

let getRegister name =
    if registers.ContainsKey name 
    then registers.Item(name) 
    else 
    registers.Add (name, 0)
    0

let mutable maxHeldValue = 0

let changeRegister expr (value : int) =
    match expr with
    | Register (name) ->
    if registers.ContainsKey name then registers.[name] <- registers.[name] + value else registers.Add (name, value)
    if registers.[name] > maxHeldValue then maxHeldValue <- registers.[name]

let rec getExpression expr =
    match expr with
    | Eq (reg, arg) -> if getExpression reg = getExpression arg then 1 else 0
    | NotEq (reg, arg) -> if getExpression reg <> getExpression arg then 1 else 0
    | Gt (reg, arg) -> if getExpression reg > getExpression arg then 1 else 0
    | Lt (reg, arg) -> if getExpression reg < getExpression arg then 1 else 0
    | Gte (reg, arg) -> if getExpression reg >= getExpression arg then 1 else 0
    | Lte (reg, arg) -> if getExpression reg <= getExpression arg then 1 else 0
    | Register (name) -> getRegister name
    | Integer (x) -> x

let rec calcStatement statement =
    match statement with
    | IfStmt (condExpr, changeExpr) -> if getExpression condExpr = 1 then calcStatement changeExpr
    | Inc (reg, arg) -> changeRegister reg (getExpression arg)
    | Dec (reg, arg) -> changeRegister reg -(getExpression arg)

statements |> Seq.iter (calcStatement)

registers |> Seq.maxBy (fun x -> x.Value)

maxHeldValue
