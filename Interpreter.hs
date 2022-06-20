-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DEL INTÉRPRETE DEL LENGUAJE DE MÁQUINA

module Interpreter where

import MachineLang

import Data.Char (chr)


type Conf = (Stack,Env)

type Env = [(Var,Integer)]
type Stack = [Integer]


interp :: Code -> Code -> Conf -> IO Conf
interp code1 code2 (stack, amb) = interpAux(code1, code2, stack, amb)


interpAux(code1, [], stack, amb) = return (stack, amb)

interpAux(code1, NEG:xs_code, fst_int:xs_stack, amb) = 
    if fst_int == 0 
        then interpAux([NEG] ++ code1, xs_code, [1] ++ xs_stack, amb)
        else interpAux([NEG] ++ code1, xs_code, [-fst_int] ++ xs_stack, amb)

interpAux(code1, ADD:xs_code, fst_int:snd_int:xs_stack, amb) =  
    interpAux([ADD] ++ code1, xs_code, [(fst_int + snd_int)] ++ xs_stack, amb)

interpAux(code1, SUB:xs_code, fst_int:snd_int:xs_stack, amb) =  
    interpAux([SUB] ++ code1, xs_code, [(fst_int - snd_int)] ++ xs_stack, amb)

interpAux(code1, MUL:xs_code, fst_int:snd_int:xs_stack, amb) =  
    interpAux([MUL] ++ code1, xs_code, [(fst_int * snd_int)] ++ xs_stack, amb)

interpAux(code1, DIV:xs_code, fst_int:snd_int:xs_stack, amb) =  
    interpAux([DIV] ++ code1, xs_code, [(fst_int `div` snd_int)] ++ xs_stack, amb)

interpAux(code1, MOD:xs_code, fst_int:snd_int:xs_stack, amb) =  
    interpAux([MOD] ++ code1, xs_code, [(fst_int `mod` snd_int)] ++ xs_stack, amb)

interpAux(code1, PUSH n:xs_code, stack, amb) =
    interpAux([PUSH n] ++ code1, xs_code, [n] ++ stack, amb)

interpAux(code1, CMP:xs_code, fst_int:snd_int:xs_stack, amb) = 
    if fst_int > snd_int
        then interpAux([CMP] ++ code1, xs_code, [1] ++ xs_stack, amb)
    else if fst_int == snd_int
        then interpAux([CMP] ++ code1, xs_code, [0] ++ xs_stack, amb)
    else
        interpAux([CMP] ++ code1, xs_code, [-1] ++ xs_stack, amb)

interpAux(code1, JUMP n:xs_code, stack, amb) = 
    if n > 0
        then
            interpAux(reverse (take (n-1) xs_code) ++ [JUMP n] ++ code1, drop (n-1) xs_code, stack, amb)
        else
            interpAux(drop (abs(n)) code1, (reverse (take (abs(n)) code1)) ++ [JUMP n] ++ xs_code, stack, amb)

interpAux(code1, JMPZ n:xs_code, fst_int:xs_stack, amb) =
    if fst_int /= 0
        then
            interpAux([JMPZ n] ++ code1, xs_code, xs_stack, amb)
        else
            if n > 0
                then
                    interpAux(reverse (take (n-1) xs_code) ++ [JMPZ n] ++ code1, drop (n-1) xs_code, xs_stack, amb)
                else
                    interpAux(drop (abs(n)) code1, reverse (take (abs(n)) code1) ++ [JMPZ n] ++ xs_code, xs_stack, amb)

interpAux(code1, STORE var:xs_code, fst_int:xs_stack, amb) =
    if variableInEnv var amb
        then
            interpAux([STORE var] ++ code1, xs_code, xs_stack, replaceValueOfVar amb var fst_int)
        else
            interpAux([STORE var] ++ code1, xs_code, xs_stack, [(var, fst_int)] ++ amb)

interpAux(code1, LOAD var:xs_code, stack, amb) =
    interpAux([LOAD var] ++ code1, xs_code, [getValueFromName var amb] ++ stack, amb)

interpAux(code1, READ:xs_code, stack, amb) = do char <- getChar
                                                interpAux([READ] ++ code1, xs_code, [(toInteger(fromEnum(char)))] ++ stack, amb)


interpAux(code1, WRITE:xs_code, fst_int:stack, amb) = do putChar(chr(fromInteger(fst_int)))
                                                         interpAux([WRITE] ++ code1, xs_code, stack, amb)

interpAux(code1, SKIP:xs_code, stack, amb) = 
    interpAux([SKIP] ++ code1, xs_code, stack, amb)

interpAux(_,_,_,_) =  return ([],[])


getValueFromName n (x:xs)
    | n == fst x = snd x
    | otherwise = getValueFromName n xs


variableInEnv n [] = False
variableInEnv n (x:xs)
    | n == fst x = True
    | otherwise = variableInEnv n xs


replaceValueOfVar [] var int = []
replaceValueOfVar (fst_amb:xs_amb) var int
    | fst fst_amb == var = [(var, int)] ++ replaceValueOfVar xs_amb var int
    | otherwise = [fst_amb] ++ replaceValueOfVar xs_amb var int