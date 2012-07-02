module FStat.Extensions



open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.FSharp


let DataMatrix (x: _ list) = x |> Array.ofList :> System.Collections.Generic.IList<_> |> DenseMatrix.CreateFromRows
let nApply      f    (X:Matrix<_>)  =  vector (X.RowEnumerator() |> Seq.map (fun (i,x) ->  f x ) |> Seq.toList)
