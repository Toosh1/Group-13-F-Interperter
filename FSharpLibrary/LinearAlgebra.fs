// Calculus Function
// Author: Joshua Allan 
// Date: <Today's Date>
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

module FSharpLibrary.LinearAlgebra
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions
    type Vector = float list
    type Matrix = float list list

    module VectorOps =
        let dotProduct (v1: Vector) (v2: Vector) =
            if List.length v1 <> List.length v2 then
                raise (ArgumentException "Vectors must have the same dimension.")
            List.zip v1 v2 |> List.sumBy (fun (x, y) -> x * y)

        let crossProduct (v1: Vector) (v2: Vector) =
            match v1, v2 with
            | [x1; y1; z1], [x2; y2; z2] -> 
                [y1 * z2 - z1 * y2; z1 * x2 - x1 * z2; x1 * y2 - y1 * x2]
            | _ -> raise (ArgumentException "Cross product is only defined for 3D vectors.")

        let norm (v: Vector) =
            Math.Sqrt(v |> List.sumBy (fun x -> x * x))

    module MatrixOps =
        let determinant (matrix: Matrix) =
            let rec det m =
                match m with
                | [[x]] -> x
                | _ ->
                    m.[0]
                    |> List.mapi (fun j el ->
                        el * det (List.tail m |> List.map (fun row -> row |> List.removeAt j)) * (if j % 2 = 0 then 1.0 else -1.0))
                    |> List.sum
            if List.isEmpty matrix || List.length matrix <> List.length (List.head matrix) then
                raise (ArgumentException "Matrix must be square.")
            det matrix

        let transpose (matrix: Matrix) =
            List.init (List.head matrix |> List.length) (fun i -> matrix |> List.map (fun row -> row.[i]))

        let multiply (m1: Matrix) (m2: Matrix) =
            if List.head m1 |> List.length <> List.length m2 then
                raise (ArgumentException "Matrix dimensions do not allow multiplication.")
            let m2T = transpose m2
            m1 |> List.map (fun row -> m2T |> List.map (VectorOps.dotProduct row))

    let floatListToString (lst: float list) =
        String.Join(",", lst |> List.map string)
    
    let floatListListToString (lst: float list list) =
        lst
        |> List.map (fun innerList -> String.Join(",", innerList |> List.map string))
        |> (fun innerStrings -> String.Join("; ", innerStrings))
        
    let vectorFromString (string: string) =
         let v = string.Split(',') |> Array.map float |> Array.toList
         v
    let matrixFromString (string: string) =
        let matrix = string.Split(';') |> Array.map (fun row -> row.Split(',') |> Array.map float |> Array.toList) |> Array.toList
        matrix
        
    let callDotProduct (vLeft: string, vRight: String) =
        let v1 = vectorFromString vLeft
        let v2 = vectorFromString vRight
        let result = VectorOps.dotProduct v1 v2
        result
        
    let callCrossProduct (vLeft: string, vRight: String) =
        let v1 = vectorFromString vLeft
        let v2 = vectorFromString vRight
        let result = floatListToString (VectorOps.crossProduct v1 v2)
        result
        
    let callNorm (vLeft: string) =
        let v1 = vectorFromString vLeft
        let result = VectorOps.norm v1
        result
        
    let callDeterminant (mLeft: string) =
        let m1 = matrixFromString mLeft
        let result = MatrixOps.determinant m1
        result
    
    let callMult (mLeft: string, mright: string) =
        let m1 = matrixFromString mLeft
        let m2 = matrixFromString mright
        let result = MatrixOps.multiply m1 m2
        floatListListToString result
        