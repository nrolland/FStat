module FStat.Extensions



open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.FSharp
open Microsoft.FSharp.Math.SI
open FStat.LinearAlgebra


let DataMatrix (x: _ list) = x |> Array.ofList :> System.Collections.Generic.IList<_> |> DenseMatrix.CreateFromRows
let nApply     (f:Vectoru<'b> -> float<'c>)    (X:Matrixu<'a,'b>)  =  Vectoru<'c> (vector (X.RowEnumerator() |> Seq.map (fun (i,x) ->  float(f x)) |> Seq.toList))
let matscale  (X:Matrix<float>) ()    = 10.



let empiricalVar (X:Matrix<float<_>>)    = (X.Transpose() * X).Divide(float X.RowCount)// * 1.<'u>





let sqr  x                 = x * x
let sqrm  (x:float<m>)     = x * x
let sqrsp (x:float<m/s>)   = x * x


//let sqr x = x * x
//let sqr x = x * x

