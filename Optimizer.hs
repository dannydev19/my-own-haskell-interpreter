-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN

module Optimizer where

import Syntax

optimize :: Program -> Program
optimize(Program mainbody) = Program (optimizeMainBody(mainbody))


stmtToComStmt :: Body -> MainBody
stmtToComStmt(stmt:xs) = [Com stmt] ++ stmtToComStmt(xs)
stmtToComStmt([]) = []


optimizeMainBody :: MainBody -> MainBody
optimizeMainBody(Com (StmtExpr expr):xs) = 
    [Com (StmtExpr (optimizeExpr(expr)))] ++ optimizeMainBody(xs)
optimizeMainBody(Com (PutChar expr):xs) =
    [Com (PutChar (optimizeExpr(expr)))] ++ optimizeMainBody(xs)
optimizeMainBody(Com (If expr fst_body snd_body):xs) =
    if exprContainsVar(optimizeExpr(expr))
        then [Com (If (optimizeExpr(expr)) (optimizeBody(fst_body)) (optimizeBody(snd_body)))] ++ optimizeMainBody(xs)
        else if (valueOfExpr(optimizeExpr(expr)) /= 0)
            then
                stmtToComStmt(optimizeBody(fst_body)) ++ optimizeMainBody(xs)
            else
                stmtToComStmt(optimizeBody(snd_body)) ++ optimizeMainBody(xs)
optimizeMainBody(Com (While expr body):xs) = 
    if exprContainsVar(optimizeExpr(expr))
        then [Com (While (optimizeExpr(expr)) (optimizeBody(body)))] ++ optimizeMainBody(xs)
        else if (valueOfExpr(optimizeExpr(expr)) == 0)
            then optimizeMainBody(xs)
            else []
optimizeMainBody(Decl (VarDef type_def name_def):xs) =
    [Decl (VarDef type_def name_def)] ++ optimizeMainBody(xs)
optimizeMainBody([]) = []


optimizeBody :: Body -> Body
optimizeBody(StmtExpr expr:xs) = [StmtExpr (optimizeExpr(expr))] ++ optimizeBody(xs)
optimizeBody(PutChar expr:xs) = [PutChar (optimizeExpr(expr))] ++ optimizeBody(xs)
optimizeBody(If expr fst_body snd_body:xs) = 
    if exprContainsVar(optimizeExpr(expr))
        then [If (optimizeExpr(expr)) (optimizeBody(fst_body)) (optimizeBody(snd_body))] ++ optimizeBody(xs)
        else if (valueOfExpr(optimizeExpr(expr)) /= 0)
            then
                optimizeBody(fst_body) ++ optimizeBody(xs)
            else
                optimizeBody(snd_body) ++ optimizeBody(xs)
optimizeBody(While expr body:xs) = 
    if exprContainsVar(optimizeExpr(expr))
        then [While (optimizeExpr(expr)) (optimizeBody(body))] ++ optimizeBody(xs)
        else if (valueOfExpr(optimizeExpr(expr)) == 0)
            then optimizeBody(xs)
            else []
optimizeBody([]) = []


optimizeExpr(Var name) = Var name
optimizeExpr(CharLit char) = CharLit char
optimizeExpr(NatLit nat) = NatLit nat
optimizeExpr(GetChar) = GetChar
optimizeExpr(Assign name expr) = Assign name (optimizeExpr(expr))


optimizeExpr(Unary uop expr) = Unary uop (optimizeExpr(expr))

optimizeExpr(Binary bop (Assign name assign_expr) expr) = Binary bop (Assign name assign_expr) (optimizeExpr(expr))

optimizeExpr(Binary Or expr (NatLit 0)) = optimizeExpr(expr)
optimizeExpr(Binary Or (NatLit 0) expr) = optimizeExpr(expr)
optimizeExpr(Binary Or expr (NatLit nat)) = NatLit nat
optimizeExpr(Binary Or (NatLit nat) expr) = NatLit nat

optimizeExpr(Binary And expr (NatLit 0)) = NatLit 0
optimizeExpr(Binary And (NatLit 0) expr) = NatLit 0
optimizeExpr(Binary And expr (NatLit nat)) = optimizeExpr(expr) 
optimizeExpr(Binary And (NatLit nat) expr) = optimizeExpr(expr)
optimizeExpr(Binary And (Binary Equ (Var fst_var) (NatLit fst_nat)) (Binary Equ (Var snd_var) (NatLit snd_nat))) =
    Binary And (Binary Equ (Var fst_var) (NatLit fst_nat)) (Binary Equ (Var snd_var) (NatLit snd_nat))



optimizeExpr(Binary Equ fst_expr snd_expr) =
    Binary Equ (optimizeExpr(fst_expr)) (optimizeExpr(snd_expr))

optimizeExpr(Binary Less fst_expr snd_expr) = 
    Binary Less (optimizeExpr(fst_expr)) (optimizeExpr(snd_expr))

optimizeExpr(Binary Plus expr (NatLit 0)) = optimizeExpr(expr)
optimizeExpr(Binary Plus (NatLit 0) expr) = optimizeExpr(expr)
optimizeExpr(Binary Plus (NatLit fst_nat) (NatLit snd_nat)) = NatLit (fst_nat + snd_nat)

optimizeExpr(Binary Minus (NatLit fst_nat) (NatLit snd_nat)) = NatLit (fst_nat - snd_nat)

optimizeExpr(Binary Mult expr (NatLit 1)) = optimizeExpr(expr)
optimizeExpr(Binary Mult (NatLit 1) expr) = optimizeExpr(expr)
optimizeExpr(Binary Mult expr (NatLit 0)) = NatLit 0
optimizeExpr(Binary Mult (NatLit 0) expr) = NatLit 0
optimizeExpr(Binary Mult (NatLit fst_nat) (NatLit snd_nat)) = NatLit (fst_nat * snd_nat)

optimizeExpr(Binary Div (NatLit fst_nat) (NatLit snd_nat)) = NatLit (fst_nat `div` snd_nat)

optimizeExpr(Binary Mod (NatLit fst_nat) (NatLit snd_nat)) = NatLit (fst_nat `mod` snd_nat)

optimizeExpr(Binary bop (Var var) (NatLit nat)) = Binary bop (Var var) (NatLit nat)
optimizeExpr(Binary bop (NatLit nat) (Var var)) = Binary bop (NatLit nat) (Var var)

optimizeExpr(Binary bop (Var fst_var) (Var snd_var)) = Binary bop (Var fst_var) (Var snd_var)

optimizeExpr(Binary Plus (Binary Mult expr (NatLit 0)) (NatLit nat)) = NatLit nat
optimizeExpr(Binary Plus (Binary Mult (NatLit 0) expr) (NatLit nat)) = NatLit nat
optimizeExpr(Binary Plus (NatLit nat) (Binary Mult expr (NatLit 0))) = NatLit nat
optimizeExpr(Binary Plus (NatLit nat) (Binary Mult (NatLit 0) expr)) = NatLit nat

optimizeExpr(Binary Plus (Var var) (Binary Mult expr (NatLit 0))) = Var var

optimizeExpr(Binary bop (Var var) expr) = Binary bop (Var var) (optimizeExpr(expr))

optimizeExpr(Binary bop expr (Var var)) = Binary bop (optimizeExpr(expr)) (Var var)

optimizeExpr(Binary bop expr (NatLit nat)) = Binary bop (optimizeExpr(expr)) (NatLit nat)

optimizeExpr(Binary bop (NatLit nat) expr) = optimizeExpr(Binary bop (NatLit nat) (optimizeExpr(expr)))

optimizeExpr(Binary bop fst_expr snd_expr) =
    optimizeExpr(Binary bop (optimizeExpr(fst_expr)) (optimizeExpr(snd_expr)))


valueOfExpr :: Expr -> Integer
valueOfExpr(NatLit nat) = nat
valueOfExpr(Unary Not expr) = 
    if ((valueOfExpr(expr)) /= 0)
        then 0
        else 1
valueOfExpr(Binary Or fst_expr snd_expr) = 
    if (valueOfExpr(fst_expr) /= 0)
        then 1
        else if (valueOfExpr(snd_expr) /= 0)
            then 1
            else 0
valueOfExpr(Binary And fst_expr snd_expr) =
    if (valueOfExpr(fst_expr) == 0)
        then 0
        else if (valueOfExpr(snd_expr) == 0)
            then 0 
            else 1
valueOfExpr(Binary Equ fst_expr snd_expr) =
    if (valueOfExpr(fst_expr) == valueOfExpr(snd_expr))
        then 1
        else 0
valueOfExpr(Binary Less fst_expr snd_expr) =
    if (valueOfExpr(fst_expr) < valueOfExpr(snd_expr))
        then 1
        else 0
valueOfExpr(Binary Plus fst_expr snd_expr) =
    valueOfExpr(fst_expr) + valueOfExpr(snd_expr)
valueOfExpr(Binary Minus fst_expr snd_expr) =
    valueOfExpr(fst_expr) - valueOfExpr(snd_expr)
valueOfExpr(Binary Mult fst_expr snd_expr) =
    valueOfExpr(fst_expr) * valueOfExpr(snd_expr)
valueOfExpr(Binary Div fst_expr snd_expr) =
    valueOfExpr(fst_expr) `div` valueOfExpr(snd_expr)
valueOfExpr(Binary Mod fst_expr snd_expr) =
    valueOfExpr(fst_expr) `mod` valueOfExpr(snd_expr)


exprContainsVar :: Expr -> Bool
exprContainsVar(Var name) = True
exprContainsVar(CharLit char) = False
exprContainsVar(NatLit nat) = False
exprContainsVar(GetChar) = False
exprContainsVar(Unary uop expr) = exprContainsVar(expr)
exprContainsVar(Binary bop fst_expr snd_expr) = or [exprContainsVar(fst_expr), exprContainsVar(snd_expr)]
exprContainsVar(Assign name var) = True