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

-- CHECK NOMBRES

checkProgram :: Program -> [Error]
checkProgram(Program mainbody) = 
   if length (checkMainBodyNombres(mainbody, [])) > 0
      then checkMainBodyNombres(mainbody, [])
      else checkMainBodyTipos(mainbody, [])



checkMainBodyNombres(Com stmt:xs, env) = checkStmtNombres(stmt, env) ++ checkMainBodyNombres(xs, env)
checkMainBodyNombres(Decl (VarDef type_def name_def):xs, env) = 
   if yaDeclarada name_def env
   then [Duplicated name_def] ++ checkMainBodyNombres(xs, env)
   else checkMainBodyNombres(xs, env ++ [(type_def, name_def)])
checkMainBodyNombres([], env) = []


checkBodyNombres(x:xs, env) = checkStmtNombres(x, env) ++ checkBodyNombres(xs, env)
checkBodyNombres([], env) = []


checkStmtNombres(StmtExpr expr, env) = checkExprNombres(expr, env)
checkStmtNombres(If expr fst_body snd_body, env) = 
   checkExprNombres(expr, env) ++ checkBodyNombres(fst_body, env) ++ checkBodyNombres(snd_body, env) 
checkStmtNombres(While expr body, env) = checkExprNombres(expr, env) ++ checkBodyNombres(body, env)
checkStmtNombres(PutChar expr, env) = checkExprNombres(expr, env)


checkExprNombres(Var vardef_name, env) =
   if yaDeclarada vardef_name env
      then []
      else [Undefined vardef_name]
checkExprNombres(Unary uop expr, env) = checkExprNombres(expr, env)
checkExprNombres(Binary bop fst_expr snd_expr, env) = 
   checkExprNombres(fst_expr, env) ++ checkExprNombres(snd_expr, env)
checkExprNombres(Assign var_name expr, env) = 
   if yaDeclarada var_name env
      then checkExprNombres(expr, env)
      else [Undefined var_name] ++ checkExprNombres(expr, env)
checkExprNombres(_, _) = []


yaDeclarada _ [] = False
yaDeclarada n (x:xs)
  | n == snd x = True
  | otherwise = yaDeclarada n xs









-- CHECK TIPOS 

getTipoFromName n (x:xs)
   | n == snd x = fst x
   | otherwise = getTipoFromName n xs


getTipo(CharLit char, env) = TyChar

getTipo(NatLit integer, env) = TyInt

getTipo(Var name, env) = 
   getTipoFromName name env

getTipo(Binary Or expr1 expr2, env) = TyInt
getTipo(Binary And expr1 expr2, env) = TyInt
getTipo(Binary Equ expr1 expr2, env) = getTipo(expr1, env)
getTipo(Binary Less expr1 expr2, env) = getTipo(expr1, env)
getTipo(Binary Plus expr1 expr2, env) = TyInt
getTipo(Binary Mult expr1 expr2, env) = TyInt
getTipo(Binary Div expr1 expr2, env) = TyInt
getTipo(Binary Mod expr1 expr2, env) = TyInt
getTipo(Binary Minus expr1 expr2, env) = TyInt

getTipo(Unary uop expr, env) =
   getTipo(expr, env)

getTipo(Assign name expr, env) =
   getTipo(expr, env)

getTipo(GetChar, env) = TyChar




--   ACA EMPIEZA LA MAGIA

checkMainBodyTipos(Com stmt:xs, env) = checkStmtTipos(stmt, env) ++ checkMainBodyTipos(xs, env)
checkMainBodyTipos(Decl (VarDef type_def name_def):xs, env) = 
   checkMainBodyTipos(xs, env ++ [(type_def, name_def)])
checkMainBodyTipos([], env) = []


checkBodyTipos(x:xs, env) = checkStmtTipos(x, env) ++ checkBodyTipos(xs, env)
checkBodyTipos([], env) = []


checkStmtTipos(StmtExpr expr, env) = checkExprTipos(expr, env)
checkStmtTipos(If expr fst_body snd_body, env) = 
   checkExprTipos(expr, env) ++ checkBodyTipos(fst_body, env) ++ checkBodyTipos(snd_body, env) 
checkStmtTipos(While expr body, env) = checkExprTipos(expr, env) ++ checkBodyTipos(body, env)
checkStmtTipos(PutChar expr, env) = 
   if (getTipo(expr, env) == TyInt)
      then checkExprTipos(expr, env) ++ [Expected TyChar TyInt]
      else checkExprTipos(expr, env)


-- cuando compara tipos diferentes
checkExprTipos(Binary Equ expr1 expr2, env) =
   if (getTipo(expr1, env) == getTipo(expr2, env))
      then checkExprTipos(expr1, env) ++ checkExprTipos(expr2, env)
   else checkExprTipos(expr1, env) ++ checkExprTipos(expr2, env) ++ [Expected (getTipo(expr1, env)) (getTipo(expr2, env))]

checkExprTipos(Binary Less expr1 expr2,env) =
   if (getTipo(expr1,env) == getTipo(expr2,env))
      then checkExprTipos(expr1,env) ++ checkExprTipos(expr2,env)
   else [Expected (getTipo(expr1,env)) (getTipo(expr2,env))]


checkExprTipos(Binary bop expr1 expr2, env) =
   if and [getTipo(expr1, env) == TyChar, getTipo(expr2, env) == TyChar]
      then 
         checkExprTipos(expr1, env) ++ checkExprTipos(expr2, env) ++ [Expected TyInt TyChar] ++ [Expected TyInt TyChar]
      else if or [getTipo(expr1, env) == TyChar, getTipo(expr2, env) == TyChar]
         then checkExprTipos(expr1, env) ++ checkExprTipos(expr2, env) ++ [Expected TyInt TyChar]
      else checkExprTipos(expr1, env) ++ checkExprTipos(expr2, env)


-- cuando asigno mal el tipo
checkExprTipos(Assign nameVar expr, env) =
   if getTipoFromName nameVar env == getTipo(expr, env)
     then checkExprTipos(expr, env)
   else  checkExprTipos(expr, env) ++ [Expected (getTipoFromName nameVar env) (getTipo(expr, env))] 

--caso recursivo y unico de unary
checkExprTipos(Unary uop expr, env) = 
   checkExprTipos(expr, env)

checkExprTipos(expr, env) = []