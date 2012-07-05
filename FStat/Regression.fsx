#if INTERACTIVE
#load @"__solmerged.fsx" 
#load @"autodisplay.fsx"
#load @"Distributions.fs" 
#load @"Extensions.fs" 
#load @"Model.fs" 
#endif

open FStat.Distributions
open FStat.Extensions
open FStat.Model
open FStat.LinearAlgebra   
open MSDN.FSharp.Charting
open System


//data
let covar       = Normal.covarDiagRot<regressor> [|1.;1.;1.|] [|-0.5*Math.PI/2.;0.3*Math.PI/2.|]
let X           = Normal.generaten 50 covar
let beta, noise = Vectoru([5.;2.;3.] |> List.toSeq), 1.<output>
let Y           = (nApply (LinearModel beta >> addnoise noise)) X
let d = DataSet(X,Y)


//models
let m           = Model(LinearModel, sumsquare >> float, LS)
let mv          = Model(LinearModel, sumsquare >> float, LSVerbose)
let mreg        = let est = LSreg 1. in Model(LinearModel, sumsquare >> float, est)



let fit =   (m.fit  d )
let fitv = (mv.fit  d )




let err  lambda  = Model(LinearModel, sumsquare >> float, LSreg lambda).kfold 5 d
let err2 lambda  = Model(LinearModel, sumsquare>> float, LSreg lambda).fit d

let lambdas = [|0. .. 0.5 .. 10. |]
lambdas |> Array.map err2
lambdas |> Array.map err


let slices max nstep = [0. .. max /float nstep .. max ]
List.zip (slices 10. 20)(slices 10. 20 |> List.map (fun l -> let m = Model(LinearModel, sumsquare>> float, LSreg(l)) in m.kfold 5  d)) |> FSharpChart.Line






//[<Measure>] type C
//[<Measure>] type F
// 
//let to_fahrenheit (x : float<C>) = x * (9.0<F>/5.0<C>) + 32.0<F>
//let to_celsius (x : float<F>) = (x - 32.0<F>) * (5.0<C>/9.0<F>)
//
//let x = to_fahrenheit 99.<C>

