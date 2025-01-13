// Calculus Function
// Author: Joshua Allan 
// Date: <Today's Date>
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

module FSharpLibrary.Calculus
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions
    
    let parsePolynomial (poly: string) : float list * int list =
        let pattern = @"([+-]?\d*\.?\d*)?(x(?:\^(\d+))?)?"
        let terms = 
            poly.Replace(" ", "")
            |> fun cleanedPoly -> Regex.Matches(cleanedPoly, pattern)
        let coefficients, powers = 
            terms 
            |> Seq.cast<Match>
            |> Seq.choose (fun m -> 
                if m.Success then
                    let coeffStr = m.Groups.[1].Value
                    let coeff = 
                        match coeffStr with
                        | "" when m.Groups.[2].Value <> "" -> 1.0  //coefficient of `1` for `x`
                        | "-" -> -1.0
                        | "+" -> 1.0
                        | "" -> 0.0  
                        | _ -> float coeffStr  
                    let powerStr = m.Groups.[3].Value
                    let power = 
                        if m.Groups.[2].Value = "" then 0  // Constants have power 0
                        else if powerStr = "" then 1  
                        else int powerStr
                    Some (coeff, power)
                else None
            )
            |> Seq.toList
            |> List.groupBy snd 
            |> List.map (fun (power, terms) -> 
                let totalCoeff = terms |> List.sumBy fst
                totalCoeff, power
            )
            |> List.unzip
        coefficients, powers

    let differentiate (coefficients: float list) (powers: int list) : float list * int list =
        coefficients
        |> List.mapi (fun i coeff -> 
            let power = powers.[i]
            if power = 0 then None
            else Some (coeff * float power, power - 1)  // Handling coefficients as floats
        )
        |> List.choose id
        |> List.unzip
    
    let rec gcd a b =
        if b = 0 then a
        else gcd b (a % b)
    let decimalToFraction (decimal: float) : int * int =
        let mutable denominator = 1
        let mutable numerator = decimal
        while (numerator % 1.0) <> 0.0 do
            numerator <- numerator * 10.0
            denominator <- denominator * 10

        let numeratorInt = int numerator
        let denominatorInt = denominator
        let commonDivisor = gcd numeratorInt denominatorInt
        numeratorInt / commonDivisor, denominatorInt / commonDivisor

    let formatCoefficient coeff =
        if coeff % 1.0 = 0.0 then
            sprintf "%.0f" coeff
        else
            let numerator,denominator = decimalToFraction coeff
            sprintf "%i/%i" numerator denominator
            
    let polynomialToString (coefficients: float list) (powers: int list) : string =
        let formatCoefficient coeff =
            // Convert to integer if the coefficient is effectively an integer
            if coeff % 1.0 = 0.0 then 
                (int coeff).ToString()
            else 
                coeff.ToString()

        coefficients
        |> List.mapi (fun i coeff ->
            let formattedCoeff = formatCoefficient coeff
            let power = powers.[i]
            match power with
            | 0 -> formattedCoeff // Constant term
            | 1 -> sprintf "%sx" formattedCoeff // Linear term
            | _ -> sprintf "%sx^%d" formattedCoeff power // Higher-order term
        )
        |> List.filter (fun term -> term <> "0") // Remove terms with coefficient 0
        |> String.concat " + "
        |> fun s -> s.Replace("+ -", "- ") // Clean up spaces around negatives



    let differentiatePolynomial (poly: string) : string =
        let coefficients, powers = parsePolynomial poly
        let newCoefficients, newPowers = differentiate coefficients powers
        polynomialToString newCoefficients newPowers
    

    // Lookup table for differentiation rules
    let differentiationRules = 
        dict [
            "sin(x)", "cos(x)"
            "cos(x)", "-sin(x)"
            "tan(x)", "sec^2(x)"
            "ln(x)", "1/x"
            "exp(x)", "exp(x)"
        ]

    // Parse expression into polynomials and special terms
    let parseExpression (expr: string) : (float list * int list) * (string * float) list =
        let specialPattern = @"([+-]?\d*\.?\d*)?(sin\(x\)|cos\(x\)|tan\(x\)|ln\(x\)|exp\(x\))"

        // Function to parse special terms
        let parseSpecialTerms (expr: string) : (string * float) list =
            let matches = Regex.Matches(expr.Replace(" ", ""), specialPattern) :> seq<Match>

            matches
            |> Seq.choose (fun m -> 
                if m.Success then
                    let coeffStr = m.Groups.[1].Value
                    let coeff = 
                        match coeffStr with
                        | "" when m.Groups.[2].Value <> "" -> 1.0
                        | "-" -> -1.0
                        | "+" -> 1.0
                        | "" -> 0.0
                        | _ -> float coeffStr
                    let term = m.Groups.[2].Value
                    if term <> "" then Some (term, coeff) else None
                else None
            )
            |> Seq.toList

        let cleanExpression = 
            Regex.Replace(expr, specialPattern, "")
            |> fun s -> s.Replace(" ", "")

        let polyCoeffs, polyPowers = parsePolynomial cleanExpression

        (polyCoeffs, polyPowers), parseSpecialTerms expr
    let differentiateSpecialTerms (terms: (string * float) list) : (string * float) list =
        terms
        |> List.collect (fun (term, coeff) ->
            match differentiationRules.TryGetValue(term) with
            | true, differentiatedTerm -> [(differentiatedTerm, coeff)]
            | false, _ -> [(term, 0.0)]
        )
        |> List.filter (snd >> ((<>) 0.0))

    // Convert special terms to string
    let specialTermsToString (terms: (string * float) list) : string =
        terms
        |> List.map (fun (term, coeff) ->
            match coeff with
            | 1.0 -> term
            | -1.0 -> "-" + term
            | _ -> sprintf "%.2f%s" coeff term
        )
        |> String.concat " + "
        |> fun s -> s.Replace("+ -", "- ")

    // Main differentiation function
    let differentiateExpression (expr: string) : string =
        let (polyCoeffs, polyPowers), specialTerms = parseExpression expr
        let newCoeffs, newPowers = differentiate polyCoeffs polyPowers
        let newSpecialTerms = differentiateSpecialTerms specialTerms
        let polyPart = polynomialToString newCoeffs newPowers
        let specialPart = specialTermsToString newSpecialTerms

        match polyPart, specialPart with
        | "", _ -> specialPart
        | _, "" -> polyPart
        | _ -> polyPart + " + " + specialPart
            
    //Chain rule
    let chainRule (outer: string) (inner: string) : string =
        let outerDerivative = differentiateExpression outer

        let replacedOuterDerivative = outerDerivative.Replace("x", $"({inner})")

        let innerDerivative = differentiateExpression inner

        sprintf "(%s) * (%s)" replacedOuterDerivative innerDerivative
    // Product Rule
    let productRule (u: string) (v: string) : string =
        let du = differentiateExpression u
        let dv = differentiateExpression v
        sprintf "(%s) * (%s) + (%s) * (%s)" du v u dv

    // Quotient Rule
    let quotientRule (u: string) (v: string) : string =
        let du = differentiateExpression u
        let dv = differentiateExpression v
        sprintf "((%s) * (%s) - (%s) * (%s)) / ((%s)^2)" du v u dv v
     //identify which rule to use
    let identifyDifferentiationRule (expr: string) : string =
        let productPattern = @"(.+)\s*\*\s*(.+)"
        let quotientPattern = @"(.+)\s*\/\s*(.+)"
        let chainPattern = @"\w+\(([^)]+)\)" 
        let sumPattern = @"(.+)\s*\+\s*(.+)"
        let subtractPattern = @"(.+)\s*-\s*(.+)"

        match expr with
        | _ when Regex.IsMatch(expr, productPattern) -> "Product Rule"
        | _ when Regex.IsMatch(expr, quotientPattern) -> "Quotient Rule"
        | _ when Regex.IsMatch(expr, chainPattern) -> "Chain Rule"
        | _ when Regex.IsMatch(expr, sumPattern) -> "Sum Rule"
        | _ when Regex.IsMatch(expr, subtractPattern) -> "Sum Rule"
        | _ -> "Polynomial Rule (basic differentiation)"

    // Polynomial integration functions
    let integrate (coefficients: float list) (powers: int list) : float list * int list =
        coefficients
        |> List.mapi (fun i coeff -> 
            let power = powers.[i]
            if power = -1 then None
            else
                 let newCoeff = coeff / float (power + 1)
                 if newCoeff = 0.0 then None 
                 else Some (newCoeff, power + 1)
        )
        |> List.choose id
        |> List.unzip

    let integratePolynomial (poly: string) : string =
        let coefficients, powers = parsePolynomial poly
        let newCoefficients, newPowers = integrate coefficients powers
        polynomialToString newCoefficients newPowers
    
    // Integration for special functions
    let integrationRules =
        dict [
            "sin(x)", "-cos(x)"
            "cos(x)", "sin(x)"
            "tan(x)", "-ln|cos(x)|"
            "ln(x)", "x * ln(x) - x"
            "exp(x)", "exp(x)"
        ]
    


    // Integration of special terms
    let integrateSpecialTerms (terms: (string * float) list) : (string * float) list =
        terms
        |> List.collect (fun (term, coeff) ->
            match integrationRules.TryGetValue(term) with
            | true, integratedTerm -> [(integratedTerm, coeff)]
            | false, _ -> [(term, 0.0)]
        )
        |> List.filter (snd >> ((<>) 0.0))



    // Main integration function
    let integrateExpression (expr: string) : string =
        let (polyCoeffs, polyPowers), specialTerms = parseExpression expr
        let newCoeffs, newPowers = integrate polyCoeffs polyPowers
        let newSpecialTerms = integrateSpecialTerms specialTerms
        let polyPart = polynomialToString newCoeffs newPowers
        let specialPart = specialTermsToString newSpecialTerms

        match polyPart, specialPart with
        | "", _ -> specialPart
        | _, "" -> polyPart
        | _ -> polyPart + " + " + specialPart       

        
    // Function to evaluate the polynomial expression at a given x
    let evaluatePolynomial (coefficients: float list) (powers: int list) (x: float) : float =
        List.zip coefficients powers
        |> List.fold (fun acc (coeff, power) -> acc + (coeff * Math.Pow(x, float power))) 0.0
        
    let definiteIntegral (poly: string) (a: float) (b: float) : float =
        let coefficients, powers = parsePolynomial poly
        
        let integralCoefficients, integralPowers = integrate coefficients powers
        
        // Evaluate the antiderivative at the upper and lower bounds
        let f_b = evaluatePolynomial integralCoefficients integralPowers b
        let f_a = evaluatePolynomial integralCoefficients integralPowers a
        
        f_b - f_a

    // Function to evaluate the derivative of the polynomial at a given x
    let evaluateDerivative (coefficients: float list) (powers: int list) (x: float) : float =
        let newCoefficients, newPowers = differentiate coefficients powers
        evaluatePolynomial newCoefficients newPowers x

    // Newton-Raphson Method for finding roots of the polynomial
    let newtonRaphson (poly: string) (x0: float) (err: float) : float =
        let coefficients, powers = parsePolynomial poly
        let mutable root = x0
        let mutable stop = false
        let mutable iterations = 0

        while not stop do
            iterations <- iterations + 1
            let f_val = evaluatePolynomial coefficients powers root
            let f_prime_val = evaluateDerivative coefficients powers root
            if f_prime_val = 0.0 then
                raise (Exception("Derivative is zero, cannot proceed with Newton-Raphson"))
            
            let newRoot = root - f_val / f_prime_val
            
            if Math.Abs(newRoot - root) < err then
                stop <- true
            
            root <- newRoot
        
        root

    //Bi-section Method
    let bisectionStep (poly: string) (a: float) (b: float) : float =
        let coefficients, powers = parsePolynomial poly

        let f_a = evaluatePolynomial coefficients powers a
        let f_b = evaluatePolynomial coefficients powers b
        
        if f_a * f_b > 0.0 then
            raise (Exception("Function has the same sign at both endpoints of the interval"))
        
        let mid = (a + b) / 2.0
        let f_mid = evaluatePolynomial coefficients powers mid
        
        if f_a * f_mid < 0.0 then
            mid 
        else
            mid 