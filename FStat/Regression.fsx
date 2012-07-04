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
open FStat.LinearAlgebra   

open MSDN.FSharp.Charting
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.FSharp
open System
open Microsoft.FSharp.Math.SI


[<Measure>] type regressor
[<Measure>] type parameter
[<Measure>] type output = regressor * parameter
[<Measure>] type nexp

//General definition
let addnoise (sigma:float<'u>) x = x + sigma * Normal.Next()
let sumsquare  (y:Vectoru<output>)                                = (y.Norm(2.)) * (y.Norm(2.)) 
let LinearModel  (beta:Vectoru<parameter>)(x:Vectoru<regressor> ) = x.DotProduct(beta)
let mLinearModel (beta:Vectoru<parameter>)                        = nApply (LinearModel beta) 


//Estimator
let LS         (X:Matrixu<regressor>) (Y:Vectoru<output>)    =  
   let precision = (X.Transpose()*X).Inverse() 
   precision * X.Transpose().Multiply(Y)

let LSVerbose  (X:Matrixu<regressor>) (Y:Vectoru<output>)    =  
   let precision = (X.Transpose()*X).Inverse()
   let a = precision * X.Transpose()
   let beta = a.Multiply(Y)
   let sigmahat =  1. / float(X.RowCount - X.ColumnCount - 1) * sumsquare (Y - mLinearModel beta X)  |> sqrt
   let spread = 1.96 * sigmahat   * (precision.Diagonal().map sqrt)
   let a = (beta - spread)
   printfn "Estimated noise %A" sigmahat 
   printfn "Estimated beta 5pct \n low  %A \n high %A" (beta - spread) ((beta + spread))
   beta

let LSreg (lambda:float<1>) (X:Matrixu<'u>) (Y:Vectoru<'v>)    = 
   let precision = (X.Transpose()*X + Matrixu<'u^2>.Identity(X.ColumnCount) * lambda ).Inverse()
   precision * X.Transpose().Multiply(Y)

let covar  = Normal.covarFromDiagAndRotation [|1.;1.;1.|] [|-0.5*Math.PI/2.;0.3*Math.PI/2.|]
let X           = Normal.generaten 50 covar |> DataMatrix<regressor>
let toto =  let arg = [5.;2.;3.] |> List.toSeq in Vectoru<parameter>(arg)
let beta, noise = Vectoru<parameter>([5.;2.;3.] |> List.toSeq), 1.<output>
let Y           = (nApply (LinearModel beta >> addnoise noise)) X
let m           = Model(LinearModel, sumsquare >> float, LS)
let mv          = Model(LinearModel, sumsquare >> float, LSVerbose)
let mreg        = let est = LSreg 1. in Model(LinearModel, sumsquare >> float, est)

let d = DataSet(X,Y)
let fit =   (m.fit  d )
let fitv = (mv.fit  d )



let err  lambda  = Model(LinearModel, sumsquare >> float, LSreg lambda).kfold 5 d
let err2 lambda  = Model(LinearModel, sumsquare>> float, LSreg lambda).fit d

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

