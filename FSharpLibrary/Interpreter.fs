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
        Add | Sub | Mul | Div | Lpar | Rpar | Rem | Pow | Num of float | Sin | Cos | Tan | Arcsin | Arccos | Arctan |Exp |Log |Pi| Euluer 
        | Var of char
        | Assign
        

    let str2lst s = [for c in s -> c] // Converts string into list of characters

    let floatVal (c:char) = (float)((int)c - (int)'0') // Converts character digit into float

    // Function for floating-point power
    let rec floatPow num exp = 
        if exp = 0.0 then 1.0
        elif exp = 1.0 then num
        else num * floatPow num (exp - 1.0)
        
    // Polynomial differentiation functions
    
   
    //Function to handle matrix/vector input and operations
    // let matrixVectorInteraction () =
    //     Console.WriteLine("Choose an operation:")
    //     Console.WriteLine("1: Dot Product (Vectors)")
    //     Console.WriteLine("2: Cross Product (Vectors)")
    //     Console.WriteLine("3: Norm (Vector)")
    //     Console.WriteLine("4: Determinant (Matrix)")
    //     Console.WriteLine("5: Matrix Multiplication")
    //     let choice = Console.ReadLine().Trim()
    //     match choice with
    //     | "1" ->
    //         Console.Write("Enter first vector (e.g., 1,2,3): ")
    //         let v1 = Console.ReadLine().Split(',') |> Array.map float |> Array.toList
    //         Console.Write("Enter second vector: ")
    //         let v2 = Console.ReadLine().Split(',') |> Array.map float |> Array.toList
    //         let result = VectorOps.dotProduct v1 v2
    //         Console.WriteLine($"Dot Product: {result}")
    //     | "2" ->
    //         Console.Write("Enter first vector (3D only): ")
    //         let v1 = Console.ReadLine().Split(',') |> Array.map float |> Array.toList
    //         Console.Write("Enter second vector (3D only): ")
    //         let v2 = Console.ReadLine().Split(',') |> Array.map float |> Array.toList
    //         let result = VectorOps.crossProduct v1 v2
    //         Console.WriteLine($"""Cross Product: {String.Join(",", result)}""")
    //     | "3" ->
    //         Console.Write("Enter vector: ")
    //         let v = Console.ReadLine().Split(',') |> Array.map float |> Array.toList
    //         let result = VectorOps.norm v
    //         Console.WriteLine($"Norm: {result}")
    //     | "4" ->
    //         Console.WriteLine("Enter matrix rows, separated by ';' (e.g., 1,2;3,4): ")
    //         let matrix = Console.ReadLine().Split(';') |> Array.map (fun row -> row.Split(',') |> Array.map float |> Array.toList) |> Array.toList
    //         let result = MatrixOps.determinant matrix
    //         Console.WriteLine($"Determinant: {result}")
    //     | "5" ->
    //         Console.WriteLine("Enter first matrix rows, separated by ';': ")
    //         let m1 = Console.ReadLine().Split(';') |> Array.map (fun row -> row.Split(',') |> Array.map float |> Array.toList) |> Array.toList
    //         Console.WriteLine("Enter second matrix rows, separated by ';': ")
    //         let m2 = Console.ReadLine().Split(';') |> Array.map (fun row -> row.Split(',') |> Array.map float |> Array.toList) |> Array.toList
    //         let result = MatrixOps.multiply m1 m2
    //         Console.WriteLine("Resulting Matrix:")
    //         result |> List.iter (fun row -> Console.WriteLine(String.Join(",", row)))
    //     | _ ->
    //         Console.WriteLine("Invalid choice.")
    // ---- Checks and errors ----
    let isblank c = System.Char.IsWhiteSpace c 
    let isdigit c = System.Char.IsDigit c
    let lexError = System.Exception("Lexer error")
    let parseError = System.Exception("Parser error")
    // ---------------------------

    
    let variables = Dictionary<char, float>()
    // Scanner for floating-point numbers, scanning digits after the decimal point
    let rec scFloat (iStr, iVal, decPlace) = 
        match iStr with
        | c :: tail when isdigit c -> scFloat(tail, iVal + (floatVal c) / decPlace, decPlace * 10.0)
        | _ -> (iStr, iVal)

    // Extended scanner to handle both integers and floating-point numbers
    let rec scNum (iStr, iVal) =
        match iStr with
        | '.'::tail -> scFloat(tail, float iVal, 10.0)
        | c :: tail when isdigit c -> scNum(tail, 10.0 * iVal + floatVal c)
        | _ -> (iStr, iVal)

    // Lexer function to tokenize the input string into terminal symbols
    let lexer input =
        let rec scan input =
            match input with
                | [] -> []
                | '+'::tail -> Add :: scan tail
                | '-'::c::tail when isdigit c -> 
                    let (iStr, iVal) = scNum(tail, -floatVal c) // Handle negative numbers properly here
                    Num (-iVal) :: scan iStr
                | '-'::tail -> Sub :: scan tail
                | '*'::tail -> Mul :: scan tail
                | '/'::tail -> Div :: scan tail
                | '('::tail -> Lpar :: scan tail
                | ')'::tail -> Rpar :: scan tail
                | '^'::tail -> Pow :: scan tail
                | '%'::tail -> Rem :: scan tail
                | '='::tail -> Assign :: scan tail
                | c :: tail when isblank c -> scan tail // Skip whitespace
                | '.'::tail -> 
                    // Handle floating point numbers starting with "."
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
  
    
    
    // Parser function based on the grammar rules
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

    // Evaluation function (modified to handle floating-point numbers)
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
                             Topt (tLst, value / tval) // Floating-point division
            | Rem :: tail -> let (tLst, tval) = F tail
                             Topt (tLst, value % tval)
            | _ -> (tList, value)
        and F tList = (NR >> Fopt) tList
        and Fopt (tList, value) =
            match tList with
            | Pow :: tail -> let (tLst, tval) = NR tail
                             Fopt (tLst, floatPow value tval) // Floating-point exponentiation
            | _ -> (tList, value)
        and NR tList =
            match tList with
            | Var name :: Assign :: tail -> // Handle variable assignment
                let (tLst, assignedValue) = E tail
                variables.[name] <- assignedValue // Assign value to the variable
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
                (tLst, Math.Log10 tval) // Future change bases
            | Num value :: tail -> (tail, value)
            | Var name :: tail -> // Retrieve variable value
                match variables.TryGetValue(name) with
                | true, value -> (tail, value) // Return the variable's value
                | false, _ -> raise (Exception(sprintf "Variable %c not defined" name))
            | Lpar :: tail -> let (tLst, tval) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, tval)
                              | _ -> raise parseError
            | _ -> raise parseError
        E tList

   
        
    // Main function to solve the input expression, now returning a float
    let solve(input) : float =
        let oList = lexer input
        let Out = parseEvaluation oList
        snd Out
        
    let overrideX(input) =
        variables.['x'] <- input
    
// Main function
    // [<EntryPoint>]
    // let main argv =
    //     Console.WriteLine("Choose an operation:")
    //     Console.WriteLine("1. Solve an expression (not implemented yet)")
    //     Console.WriteLine("2. Differentiate a polynomial")
    //     Console.WriteLine("3. Integrate a polynomial")
    //     Console.WriteLine("4. Matrix and vector operations")
    //     
    //     let choice = Console.ReadLine().Trim()
    //     
    //     match choice with
    //     | "1" -> 
    //         Console.WriteLine("Expression solving feature is not implemented yet.")
    //     | "2" ->
    //         // Differentiation options menu
    //         Console.WriteLine("Choose a differentiation method:")
    //         Console.WriteLine("1. Polynomial or Expression Differentiation")
    //         Console.WriteLine("2. Product Rule Differentiation")
    //         Console.WriteLine("3. Quotient Rule Differentiation")
    //         Console.WriteLine("4. Chain Rule Differentiation")
    //         Console.Write("Enter your choice (1/2/3/4): ")
    //         let differentiationChoice = Console.ReadLine().Trim()
    //
    //         match differentiationChoice with
    //         | "1" ->
    //             // Polynomial differentiation
    //             Console.Write("Enter a polynomial or expression to differentiate (e.g., 3x + 3x^2 + sin(x) + cos(x)): ")
    //             let exprInput = Console.ReadLine().Trim()
    //
    //             let differentiatedResult = differentiateExpression exprInput
    //             Console.WriteLine($"Differentiated result: {differentiatedResult}")
    //
    //             Console.WriteLine("Would you like to calculate a root? (yes/no)")
    //             let calculateRoot = Console.ReadLine().Trim().ToLower()
    //
    //             match calculateRoot with
    //             | "yes" ->
    //                 Console.Write("Enter the initial guess for the root (x0): ")
    //                 let x0 = float (Console.ReadLine().Trim())
    //                 Console.Write("Enter the acceptable error (Err): ")
    //                 let err = float (Console.ReadLine().Trim())
    //                 
    //                 // Use Newton-Raphson to calculate the root
    //                 let root = newtonRaphson exprInput x0 err
    //                 Console.WriteLine($"Root found: {root}")
    //             | _ -> 
    //                 Console.WriteLine("Root calculation skipped.")
    //
    //         | "2" ->
    //             // Product Rule differentiation
    //             Console.Write("Enter the first function (u): ")
    //             let u = Console.ReadLine().Trim()
    //             Console.Write("Enter the second function (v): ")
    //             let v = Console.ReadLine().Trim()
    //             
    //             let productResult = productRule u v
    //             Console.WriteLine($"Derivative using the product rule: {productResult}")
    //
    //         | "3" ->
    //             // Quotient Rule differentiation
    //             Console.Write("Enter the numerator function (u): ")
    //             let u = Console.ReadLine().Trim()
    //             Console.Write("Enter the denominator function (v): ")
    //             let v = Console.ReadLine().Trim()
    //
    //             let quotientResult = quotientRule u v
    //             Console.WriteLine($"Derivative using the quotient rule: {quotientResult}")
    //         | "4" ->
    //             // Chain Rule differentiation
    //             Console.Write("Enter the outer function (f): ")
    //             let outer = Console.ReadLine().Trim()
    //             Console.Write("Enter the inner function (g): ")
    //             let inner = Console.ReadLine().Trim()
    //
    //             let chainResult = chainRule outer inner
    //             Console.WriteLine($"Derivative using the chain rule: {chainResult}")
    //
    //
    //         | _ -> 
    //             Console.WriteLine("Invalid choice. Returning to the main menu.")
    //         
    //     | "3" -> 
    //                 // Polynomial integration
    //                 Console.Write("Enter a polynomial to integrate (e.g., 3x + 3x^2): ")
    //                 let polyInput = Console.ReadLine().Trim()
    //                 
    //                 Console.WriteLine("Do you want to calculate the definite integral? (yes/no)")
    //                 let calculateDefinite = Console.ReadLine().Trim().ToLower()
    //                 
    //                 match calculateDefinite with
    //                 | "yes" ->
    //                     Console.Write("Enter the lower bound (a): ")
    //                     let a = float (Console.ReadLine().Trim())
    //                     Console.Write("Enter the upper bound (b): ")
    //                     let b = float (Console.ReadLine().Trim())
    //                     
    //                     let definiteResult = definiteIntegral polyInput a b
    //                     Console.WriteLine($"The definite integral of {polyInput} from {a} to {b} is: {definiteResult}")
    //                 | _ ->
    //                     let integratedResult = integratePolynomial polyInput
    //                     Console.WriteLine($"Integrated result: {integratedResult}")
    //         
    //     | "4" -> 
    //         // Matrix and vector operations
    //         matrixVectorInteraction() 
    //         
    //     | _ -> 
    //         Console.WriteLine("Invalid input. Please choose 1, 2, 3, or 4.")
    //
    //     0