module FStat.Extensions

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.FSharp
open Microsoft.FSharp.Math.SI
open FStat.LinearAlgebra

//let DataMatrix<[<Measure>]'u> (x: _ list)     = Matrixu<'u> (x |> Array.ofList :> System.Collections.Generic.IList<_> |> DenseMatrix.CreateFromRows)
let nApply     (f:Vectoru<'a> -> float<'c>) (X:Matrixu<'b>) =  Vectoru<'c> (vector (X.RowEnumerator() |> Seq.map (fun (i,x) ->  float(f x)) |> Seq.toList))

let empiricalVarCol (X:Matrixu<'v>)    =  (X.Transpose() * X).Divide(float X.RowCount   )
let empiricalVarRow (X:Matrixu<'v>)    =  (X * X.Transpose()).Divide(float X.ColumnCount)


