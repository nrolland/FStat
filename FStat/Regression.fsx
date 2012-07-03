#if INTERACTIVE
#load @"__solmerged.fsx" 
#load @"autodisplay.fsx"
#load @"Distributions.fs" 
#load @"Extensions.fs" 
#load @"Model.fs" 
#endif
open System.Windows
open FStat.Distributions
open FStat.Extensions
open FStat.Model
open MSDN.FSharp.Charting
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

//General definition
let addnoise  sigma x =      let g = Normal.Next() in  (x + sigma * g)
let addnoise2 sigma   = (+) (let g = Normal.Next() in  (printfn "NOISE %A" g; sigma * g))  // wrong
let rss              (y:Vector<float>)                        = y.Norm(2.)/float y.Count
let LinearModel   (beta:Vector<_>)  (x:Vector<_>) =  x.DotProduct(beta)

//Estimator
let LS                   (X:Matrix<_>) (Y:Vector<_>)  = (X.Transpose()*X).Inverse() * X.Transpose().Multiply(Y)
let LSreg (lambda:float) (X:Matrix<_>) (Y:Vector<_>)  = (X.Transpose()*X + lambda * DenseMatrix.Identity(X.ColumnCount) ).Inverse() * X.Transpose().Multiply(Y)


let covar  = Normal.covarFromDiagAndRotation [|5.;1.;3.|] [|-0.5*System.Math.PI/2.;0.3*System.Math.PI/2.|]

let X     = Normal.generaten 1000 covar |> DataMatrix
let beta  = DenseVector( [|5.;2.;3.|])
let noise = 10.
let Y     = nApply (LinearModel beta >> addnoise noise) X
let m = Model(LS,LinearModel, rss)
Y.Sum() / float Y.Count

//Cross valid
let d = DataSet(X,Y)
//m.structErr d d 
let fit = (m.fit  d ).ToArray()
float X.RowCount * m.structErr d d 

let sigmahat = float X.RowCount/float(X.RowCount - X.ColumnCount - 1 ) * m.structErr d d 





let slices max nstep = [0. .. max /float nstep .. max ]
List.zip (slices 10. 20)(slices 10. 20 |> List.map (fun l -> let m = Model(LSreg(l),LinearModel, rss) in m.kfold 5  d)) |> FSharpChart.Line

[<Measure>] type C
[<Measure>] type F
 
let to_fahrenheit (x : float<C>) = x * (9.0<F>/5.0<C>) + 32.0<F>
let to_celsius (x : float<F>) = (x - 32.0<F>) * (5.0<C>/9.0<F>)

let x = to_fahrenheit 99.<C>

