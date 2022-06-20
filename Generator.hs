-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE GENERACIÓN DE CÓDIGO DE MÁQUINA

module Generator where

import Syntax
import MachineLang

-- Implementar
generate :: Program -> Code
generate (Program mainbody) = generateMainbody(mainbody)

-- MAIN BODY

generateMainbody (Com stmt:xs) = generateStmp(stmt) ++ generateMainbody(xs)
generateMainbody (Decl (VarDef typeVar nameVar):xs) = generateMainbody(xs)
generateMainbody (_) = []

-- BODY

generateBody(x:xs) = generateStmp(x) ++ generateBody(xs)
generateBody[] = []

-- STMT

generateStmp (StmtExpr expr) = generateExpr expr

generateStmp (If (Binary Equ expr1 expr2) fst_body snd_body) = 
    generateExpr expr2 ++ generateExpr expr1 ++ 
    [CMP] ++ [JMPZ (length (generateBody snd_body)+2) ] ++ generateBody snd_body ++
     [JUMP (length (generateBody fst_body)+1)] ++  generateBody fst_body

generateStmp (If (Binary Less expr1 expr2) fst_body snd_body) = generateExpr expr2 ++
    generateExpr expr1 ++ [CMP] ++ [PUSH 1] ++ [ADD] ++ [JMPZ (length (generateBody snd_body)+2) ] ++ generateBody snd_body ++
     [JUMP (length (generateBody fst_body)+1)] ++  generateBody fst_body

generateStmp(If (Unary uop expr) fst_body snd_body) = 
    generateExpr expr ++ [NEG] ++ [JMPZ (length (generateBody fst_body)+2)] ++
    generateBody fst_body ++ [JUMP (length (generateBody snd_body)+1)] ++ generateBody snd_body

generateStmp (If expr fst_body snd_body) =
    generateExpr expr ++ [JMPZ (length (generateBody fst_body)+2)] ++
    generateBody fst_body ++ [JUMP (length (generateBody snd_body)+1)] ++ generateBody snd_body

generateStmp (While (Binary Equ expr1 expr2) body) = 
    generateExpr(expr1) ++ generateExpr(expr2)
    ++ [CMP] ++ [JMPZ 2, JUMP (length(generateBody(body)) + 2)]
    ++ generateBody(body)
    ++ [JUMP (-(length(generateBody(body)) + 3 + length(generateExpr(expr2)) + length(generateExpr(expr1))))]
    ++ [SKIP]

generateStmp (While (Binary Less expr1 expr2) body) =
    generateExpr(expr2) ++ generateExpr(expr1)
    ++ [CMP, PUSH 1, ADD, JMPZ 2]
    ++ [JUMP (length(generateBody(body)) + 2)] 
    ++ generateBody(body)
    ++ [JUMP (-(length(generateBody(body)) + 5 + length(generateExpr(expr1)) + length(generateExpr(expr2))))]

generateStmp (While expr body) =
    generateExpr(expr) ++
    [JMPZ (length (generateBody(body)) + 2)] ++ generateBody(body)
    ++ [JUMP (-(length(generateBody(body)) + 1 + length(generateExpr(expr))))]

generateStmp (PutChar (CharLit char)) = [PUSH (toInteger(fromEnum char))] ++ [WRITE]
generateStmp (PutChar expr) = generateExpr expr ++ [WRITE]

-- EXPR

generateExpr (Var name) = [LOAD name]
generateExpr (CharLit char1) = [PUSH (toInteger(fromEnum char1))]
generateExpr (NatLit integer) =  [PUSH integer]
generateExpr GetChar = [READ]
generateExpr (Unary Neg expr) = generateExpr(expr) ++ [NEG]
generateExpr (Unary Not expr) = generateExpr(expr) ++ [JMPZ 3, PUSH 0, JUMP 2, PUSH 1, SKIP]

generateExpr (Assign fst_var (Assign snd_var expr)) = 
    generateExpr expr ++ [STORE snd_var, LOAD snd_var, STORE fst_var]

generateExpr (Assign name expr) =  generateExpr expr ++ [STORE name, LOAD name]

generateExpr (Binary Or expr1 expr2) =
    generateExpr expr1 ++ generateExpr expr2 ++
    [ADD, JMPZ 3, PUSH 1, JUMP 2, PUSH 0, SKIP]

generateExpr (Binary And expr1 expr2) =
    generateExpr expr1 ++ generateExpr expr2 ++
    [ADD, PUSH 2, CMP, JMPZ 3, PUSH 0, JUMP 2, PUSH 1, SKIP]

generateExpr (Binary Equ expr1 expr2) = 
    generateExpr expr2 ++ generateExpr expr1 ++
    [CMP, JMPZ 3, PUSH 0, JUMP 2, PUSH 1, SKIP]

generateExpr (Binary Less expr1 expr2) = 
    generateExpr expr2 ++ generateExpr expr1  ++ [CMP]

generateExpr (Binary Plus expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [ADD]
generateExpr (Binary Minus expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [SUB]
generateExpr (Binary Mult expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [MUL]
generateExpr (Binary Div expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [DIV]
generateExpr (Binary Mod expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [MOD]
