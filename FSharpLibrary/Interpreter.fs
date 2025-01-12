// Simple Interpreter in F# 
// Author: R.J. Lapeer + Mateusz Gorecki 
// Modifications by: Joshua Allan
// Date: <Today's Date>
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report


module public interpreter
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions

    // Define terminal symbols, now supporting floating-point numbers (Num of float)
    type terminal = 
        Add | Sub | Mul | Div | Lpar | Rpar | Rem | Pow | Num of float | Sin | Cos | Tan | Arcsin | Arccos | Arctan |Exp |Log |Pi| Euluer | Csc | Cot | Sec
        | Var of char
        | Assign
        

    let str2lst s = [for c in s -> c] // Converts string into list of characters

    let floatVal (c:char) = (float)((int)c - (int)'0') // Converts character digit into float

    // Function for floating-point power
    let rec floatPow num exp = 
        if exp = 0.0 then 1.0
        elif exp = 1.0 then num
        else num * floatPow num (exp - 1.0)
        
  
    // ---- Checks and errors ----
    let isblank c = System.Char.IsWhiteSpace c 
    let isdigit c = System.Char.IsDigit c
    let lexError = System.Exception("Lexer error")
    let parseError = System.Exception("Parser error")
    // ---------------------------

    
    let variables = Dictionary<char, float>()
    let rec scFloat (iStr, iVal, decPlace) = 
        match iStr with
        | c :: tail when isdigit c -> scFloat(tail, iVal + (floatVal c) / decPlace, decPlace * 10.0)
        | _ -> (iStr, iVal)
    let rec scNum (iStr, iVal) =
        match iStr with
        | '.'::tail -> scFloat(tail, float iVal, 10.0)
        | c :: tail when isdigit c -> scNum(tail, 10.0 * iVal + floatVal c)
        | _ -> (iStr, iVal)

    let lexer input =
        let rec scan input =
            match input with
                | [] -> []
                | '+'::tail -> Add :: scan tail
                | '-'::c::tail when isdigit c -> 
                    let (iStr, iVal) = scNum(tail, -floatVal c)
                    Num (iVal) :: scan iStr
                | '-'::tail -> Sub :: scan tail
                | '*'::tail -> Mul :: scan tail
                | '/'::tail -> Div :: scan tail
                | '('::tail -> Lpar :: scan tail
                | ')'::tail -> Rpar :: scan tail
                | '^'::tail -> Pow :: scan tail
                | '%'::tail -> Rem :: scan tail
                | '='::tail -> Assign :: scan tail
                | c :: tail when isblank c -> scan tail 
                | '.'::tail -> 
                    let (iStr, iVal) = scFloat(tail, 0.0, 10.0)
                    Num iVal :: scan iStr
                | c :: tail when isdigit c -> 
                    let (iStr, iVal) = scNum(tail, floatVal c)
                    match iStr with
                    | x::xs when Char.IsLetter x -> Num iVal :: Mul :: Var x :: scan xs
                    | _ -> Num iVal :: scan iStr
                | 's'::'i'::'n'::tail -> Sin :: scan tail 
                | 'c'::'o'::'s'::tail -> Cos :: scan tail 
                | 't'::'a'::'n'::tail -> Tan :: scan tail
                | 's'::'e'::'c'::tail -> Sec :: scan tail
                | 'c'::'o'::'t'::tail -> Cot :: scan tail
                | 'c'::'s'::'c'::tail -> Csc :: scan tail
                | 'e'::'x'::'p'::tail -> Exp :: scan tail 
                | 'l'::'o'::'g'::tail -> Log :: scan tail
                | 'a'::'r'::'c'::'s'::'i'::'n'::tail -> Arcsin :: scan tail
                | 'a'::'r'::'c'::'c'::'o'::'s'::tail -> Arccos :: scan tail
                | 'a'::'r'::'c'::'t'::'a'::'n'::tail -> Arctan :: scan tail
                | 'π'::tail -> Num 3.1415926535 :: scan tail
                | 'e'::tail -> Num 2.71828 :: scan tail
                | c :: tail when Char.IsLetter c -> Var c :: scan tail
                | _ -> raise lexError
        scan (str2lst input)


    // Function to find roots of a polynomial using Newton's method
    
        


    //BNF
    // <Assignment>       ::= <Variable> "=" <Expression>
    // <Expression>       ::= <Term> <ExpressionOpt>
    // <ExpressionOpt>    ::= "+" <Term> <ExpressionOpt> | 
    //                        "-" <Term> <ExpressionOpt> | 
    //                        
    // <Term>            ::= <Factor> <TermOpt>
    // <TermOpt>         ::= "*" <Factor> <TermOpt> | 
    //                        "/" <Factor> <TermOpt> | 
    //                        "%" <Factor> <TermOpt> | 
    //                        
    // <Factor>          ::= <Base> <FactorOpt>
    // <FactorOpt>       ::= "^" <Base> <FactorOpt> | 
    //                        
    // <Base>            ::= "Num" | "(" <Expression> ")"
  
    
    let parser tList =
        let rec A tList =  
            match tList with
            | Var name :: Assign :: tail -> 
                let tailAfterExpr = E tail
                tailAfterExpr
            | _ -> E tList 
        and E tList = (T >> Eopt) tList 
        and Eopt tList =
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList
        and T tList = (F >> Topt) tList
        and Topt tList =
            match tList with
            | Mul :: tail -> (F >> Topt) tail
            | Div :: tail -> (F >> Topt) tail
            | Rem :: tail -> (F >> Topt) tail
            | _ -> tList
        and F tList = (NR >> Fopt) tList
        and Fopt tList =
            match tList with
            | Pow :: tail -> (NR >> Fopt) tail
            | _ -> tList
        and NR tList =
             match tList with
             | Sin :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Cos :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Tan :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Csc :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Cot :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Sec :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Arcsin :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Arccos :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Arctan :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Log :: tail -> 
                let tailAfterExpr = F tail
                tailAfterExpr
             | Num value :: tail -> tail
             | Var name :: tail -> tail
             | Lpar :: tail ->
                    match E tail with
                    | Rpar :: tail -> tail
                    | _ -> raise parseError
             | _ -> raise parseError
                
        A tList
    
   

    let rec parseEvaluation tList = 
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, value) = 
            match tList with
            | Add :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, value + tval)
            | Sub :: tail -> let (tLst, tval) = T tail
                             Eopt (tLst, value - tval)
            | _ -> (tList, value)
        and T tList = (F >> Topt) tList
        and Topt (tList, value) =
            match tList with
            | Mul :: tail -> let (tLst, tval) = F tail
                             Topt (tLst, value * tval)
            | Div :: tail -> let (tLst, tval) = F tail
                             if tval = 0.0 then
                                 raise (Exception("Error: Division by 0"))
                             else
                                 Topt (tLst, value / tval) 
            | Rem :: tail -> let (tLst, tval) = F tail
                             if tval = 0.0 then
                                raise (Exception("Error: Division by 0"))
                             else
                                Topt (tLst, value % tval)
            | _ -> (tList, value)
        and F tList = (NR >> Fopt) tList
        and Fopt (tList, value) =
            match tList with
            | Pow :: tail -> let (tLst, tval) = NR tail
                             Fopt (tLst, floatPow value tval)
            | _ -> (tList, value)
        and NR tList =
            match tList with
            | Var name :: Assign :: tail -> 
                let (tLst, assignedValue) = E tail
                variables.[name] <- assignedValue
                (tLst , assignedValue)
            | Sin :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Sin tval)
            | Cos :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Cos tval)
            | Tan :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Tan tval)
            | Csc :: tail -> 
                let (tLst, tval) = F tail
                (tLst, 1.0 / (Math.Sin tval))
            | Cot :: tail -> 
                let (tLst, tval) = F tail
                (tLst, 1.0 / (Math.Tan tval))
            | Sec :: tail -> 
                let (tLst, tval) = F tail
                (tLst, 1.0 / (Math.Cos tval))
            | Arcsin :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Asin tval)
            | Arccos :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Acos tval)
            | Arctan :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Atan tval)
            | Log :: tail -> 
                let (tLst, tval) = F tail
                (tLst, Math.Log10 tval)
            | Num value :: tail -> (tail, value)
            | Var name :: tail -> 
                match variables.TryGetValue(name) with
                | true, value -> (tail, value) 
                | false, _ -> raise (Exception(sprintf "Variable %c not defined" name))
            | Lpar :: tail -> let (tLst, tval) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, tval)
                              | _ -> raise parseError
            | _ -> raise parseError
        E tList

   
    let solve(input) : float =
        let oList = lexer input
        let Out = parseEvaluation oList 
        snd Out
        
    let overrideX(input) =
        variables.['x'] <- input
 