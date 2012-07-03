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
open MathNet.Numerics.FSharp
open System

//General definition
let addnoise  sigma x = x + sigma * Normal.Next()
let sumsquare     (y:Vector<float>)               =  pown (y.Norm(2.)) 2 
let LinearModel   (beta:Vector<_>)  (x:Vector<_>) =  x.DotProduct(beta)

//Estimator
let LS                   (X:Matrix<_>) (Y:Vector<_>)  = let precision = (X.Transpose()*X).Inverse() 
                                                        let beta = precision * X.Transpose().Multiply(Y)
                                                        beta

let LSVerbose            (X:Matrix<_>) (y:Vector<_>)  = let precision = (X.Transpose()*X).Inverse()
                                                        let beta = precision * X.Transpose().Multiply(y)   
                                                        let sigmahat =  1. / float(X.RowCount - X.ColumnCount - 1) * sumsquare (y - X * beta)  |> sqrt
                                                        let spread = 1.96 * sigmahat * precision.Diagonal() 
                                                        printfn "Estimated noise %A" sigmahat 
                                                        printfn "Estimated beta 5pct \n low  %A \n high %A" ((beta - spread).ToArray()) ((beta + spread).ToArray())
                                                        beta

let LSreg (lambda:float) (X:Matrix<_>) (Y:Vector<_>)  = let precision = (X.Transpose()*X + lambda * DenseMatrix.Identity(X.ColumnCount) ).Inverse()
                                                        precision * X.Transpose().Multiply(Y)

let covar  = Normal.covarFromDiagAndRotation [|1.;1.;1.|] [|-0.5*Math.PI/2.;0.3*Math.PI/2.|]

let X           = Normal.generaten 50 covar |> DataMatrix
let beta, noise = DenseVector( [|5.;2.;3.|]), 1.
let Y           = nApply (LinearModel beta >> addnoise noise) X
let m           = Model(LinearModel, sumsquare, LS)
let mv          = Model(LinearModel, sumsquare, LSVerbose)
let mreg        = Model(LinearModel, sumsquare, LSreg 1.)

let d = DataSet(X,Y)
let fit =   (m.fit  d ).ToArray()
let fitv = (mv.fit  d ).ToArray()



let err  lambda  = Model(LinearModel, sumsquare, LSreg lambda).kfold 5 d
let err2 lambda  = (Model(LinearModel, sumsquare, LSreg lambda).fit d).ToArray()

let lambdas = [|0. .. 0.5 .. 10. |]
lambdas |> Array.map err2
lambdas |> Array.map err


let slices max nstep = [0. .. max /float nstep .. max ]
List.zip (slices 10. 20)(slices 10. 20 |> List.map (fun l -> let m = Model(LinearModel, sumsquare, LSreg(l)) in m.kfold 5  d)) |> FSharpChart.Line

[<Measure>] type C
[<Measure>] type F
 
let to_fahrenheit (x : float<C>) = x * (9.0<F>/5.0<C>) + 32.0<F>
let to_celsius (x : float<F>) = (x - 32.0<F>) * (5.0<C>/9.0<F>)

let x = to_fahrenheit 99.<C>

