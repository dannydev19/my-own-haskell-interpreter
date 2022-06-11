-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

module TypeChecker where

import Syntax


data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
   show (Duplicated      n)  = "Duplicated definition: " ++ n
   show (Undefined       n)  = "Undefined: " ++ n
   show (Expected    ty ty') = "Expected: " ++ show ty
                             ++ " Actual: " ++ show ty'

type Env = [(Name, Type)]

-- Implementar
checkProgram :: Program -> [Error]
checkProgram prg = checkNombres(prg)

--checkNombres :: Program -> [Error]
checkNombres(Program mainbody) = checkMainBody(mainbody, [])

-- chequeo los duplicated
checkMainBody(Decl (VarDef vardef_type vardef_name):xs, env) =
   if yaDeclarada vardef_name env
      then [Duplicated vardef_name] ++ checkMainBody(xs, env)
      else checkMainBody(xs, env ++ [vardef_name])

-- chequeo los undefined
checkMainBody(Com (StmtExpr expr):xs, env) = checkExpr(expr, env) ++ checkMainBody(xs, env)

checkMainBody(_, _) = []


checkExpr(Var vardef_name, env) =
   if yaDeclarada vardef_name env
      then []
      else [Undefined vardef_name]

checkExpr(Unary uop expr, env) = checkExpr(expr, env)

checkExpr(Binary bop fst_expr snd_expr, env) = checkExpr(fst_expr, env) ++ checkExpr(snd_expr, env)

checkExpr(Assign var_name expr, env) = 
   if yaDeclarada var_name env
      then checkExpr(expr, env)
      else [Undefined var_name] ++ checkExpr(expr, env)

checkExpr(_, _) = []


yaDeclarada :: (Eq a) => a -> [a] -> Bool
yaDeclarada _ [] = False
yaDeclarada n (x:xs)
  | n == x = True
  | otherwise = yaDeclarada n xs


checkTipos :: Program -> [Error]
checkTipos = undefined



