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



//First exemple
let empiricalVar     (X:Matrix<_>)                                   = (X.Transpose() * X).Divide(float X.RowCount)
let addnoise sigma x  = x + sigma * (Normal.sampleNormal() |> Seq.head)
let rss              (y:Vector<float>)                        = y.Norm(2.)/float y.Count
let forecastlin      (beta:Vector<_>)  (x:Vector<_>) =  x.DotProduct(beta)
//estimator
let leastsquare (X:Matrix<_>)     (Y:Vector<_>)  = (X.Transpose()*X).Inverse() * X.Transpose().Multiply(Y)
let regLS       (lambda:float) (X:Matrix<_>)     (Y:Vector<_>)  = (X.Transpose()*X + lambda * DenseMatrix.Identity(X.ColumnCount) ).Inverse() * X.Transpose().Multiply(Y)

let covar  = let a = DenseMatrix.diag(DenseVector( [|5.;1.;3.|])) 
             let c = Normal.rotationtoR [|-0.5*System.Math.PI/2.;0.3*System.Math.PI/2.|]
             (a * c) * (a * c).Transpose()

let X     = Normal.generaten 100 covar |> DataMatrix
let beta  = DenseVector( [|5.;1.;3.|])
let noise = 1.
let Y        = nApply (forecastlin beta >> addnoise noise) X


//Cross valid
let d = DataSet(X,Y)
let m = Model(leastsquare,forecastlin, rss)


let slices max nstep = [0. .. max /float nstep .. max ]
List.zip (slices 10. 20)(slices 10. 20 |> List.map (fun l -> let m = Model(regLS(l),forecastlin, rss) in m.kfold 5  d)) |> FSharpChart.Line
