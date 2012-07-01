#if INTERACTIVE
#load @"__solmerged.fsx"
#load @"graph.fsx"
#load @"autodisplay.fsx"
#endif
open Graph
open Graph.Display2D
open System.Windows
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.Random
open MSDN.FSharp.Charting

module Normal = 
   let rnd = new MersenneTwister()
   let sampleNormal = 
      fun () -> let rec randomNormal() = let u1, u2 = rnd.NextDouble(),rnd.NextDouble()
                                         let r, theta= sqrt (-2. * (log u1)), 2. * System.Math.PI * u2  
                                         seq { yield r * sin theta; yield r * cos theta ; yield! randomNormal() }
                randomNormal()
   let private mapply (m:Generic.Matrix<float>) f = m.IndexedEnumerator() |> Seq.iter(fun (i,j,v) -> m.[i,j] <- f v ); m
   let generate (covar:Generic.Matrix<float>) = 
      let R = if covar.Determinant() = 0. 
              then  let u, d, vt = let t = covar.Svd(true) in t.U(), t.W(), t.VT()   //printfn "svd ok = %A  %A" (covar) ((u * d  * vt))             printfn "symetric ok = %A %A" (covar)( (u * d  * u.Transpose()))
                    let A = (mapply d sqrt) * u.Transpose()                          //printfn "rd ok = %A %A" (d )( rd*rd)               printfn "covarrd ok = %A %A" (covar )( u*rd.Transpose()*rd*u.Transpose())
                    let qr = A.QR() in qr.R.Transpose()                              //printfn "covar ok = %A %A" (covar)((q*r).Transpose()*(q*r))               printfn "covar2 ok = %A %A" (covar)(r.Transpose()*r)
               else let chol = covar.Cholesky()
                    chol.Factor
      fun () -> let v = vector ( sampleNormal() |> Seq.take(covar.ColumnCount) |> List.ofSeq )
                R * v
   let generaten n covar = 
         let generatecovar = generate covar
         List.init n (fun _ -> generatecovar ()) 

type DataSet  = DataSet of Generic.Matrix<float> * Generic.Vector<float>
   with static member (+) (a:DataSet,b:DataSet):DataSet  = let (DataSet(x,y)), (DataSet(u,v)) = a, b
                                                           let xx = x.Transpose().Append(u.Transpose()).Transpose() 
                                                           let yy = (y.ToRowMatrix().Append(v.ToRowMatrix())).Row(0)
                                                           DataSet(xx,yy)
        static member merge (ar :DataSet seq) : DataSet  = let   head = ar |> Seq.head
                                                           let others = ar |> Seq.skip 1
                                                           others |> Seq.fold (fun st el -> st + el) head
        member this.Splitat n = let (DataSet(x,y)) = this
                                DataSet(x.SubMatrix(0, n, 0 ,x.ColumnCount), y.SubVector(0, n )),
                                DataSet(x.SubMatrix(n,x.RowCount,0, x.ColumnCount), y.SubVector(n, x.RowCount))
        member this.Splitin k = let (DataSet(x,y)) = this
                                let step, rem = (x.RowCount) / k, (x.RowCount- (x.RowCount) / k * k)
                                seq { for i in seq {0 ..  k-1 } ->
                                         let s, e = i * step, step  + if i = k-1 then rem else 0
                                         DataSet(x.SubMatrix(s,e,0,x.ColumnCount), y.SubVector(s,e)) }    
        member this.DrawCount= let (DataSet(x,y)) = this
                               x.RowCount

type  Rotation (thetas:float array) =
   let value = lazy( let covar = Double.DenseMatrix.Identity(1+thetas.Length)
                     thetas |> Array.iteri (fun i theta -> seq { 0 .. i } |> Seq.iter(fun j -> covar.[1+i,j] <- covar.[i,j] * sin theta )
                                                           covar.[1+i,1+i] <- cos theta )
                     covar)
   member this.R = value.Value

let DataMatrix (x: _ list) = x |> Array.ofList :> System.Collections.Generic.IList<_> |> Double.DenseMatrix.CreateFromRows
let nApply      f    (X:Generic.Matrix<_>)  =  vector (X.RowEnumerator() |> Seq.map (fun (i,x) ->  f x ) |> Seq.toList)

type Model (estimationparam, forecast:Vector<float>->Vector<float>-> float, loss:Vector<float>-> float) =
   member this.structErr  (DataSet(xtrain,ytrain)) (DataSet(xtest,ytest)) =
      let p = estimationparam xtrain ytrain
      let yhat = nApply (forecast p)  xtest
      let r = loss(ytest - yhat)
      //printfn "LOSS %A YHAT %A BETA %A" r yhat p
      r

   member this.kfold (k:int) (dataset:DataSet) = 
      let artests, artrains = let ds = dataset.Splitin k in 
                              ds, let nds = ds |> Seq.mapi(fun i it -> (i,it)) in
                                  nds |> Seq.mapi (fun i s ->  DataSet.merge (nds |> Seq.filter(fun (j,s) -> not (i = j))|> Seq.map (snd))) 
   

      let results = Seq.zip artrains artests |> Seq.map(fun (train, test) -> //printfn "Train set %A \n Test set %A " train test 
                                                                             this.structErr train test)
      results |> Seq.average




//First exemple
let empiricalVar     (X:Generic.Matrix<_>)                                   = (X.Transpose() * X).Divide(float X.RowCount)
let addnoise sigma x  = x + sigma * (Normal.sampleNormal() |> Seq.head)
let rss              (y:Generic.Vector<float>)                        = y.Norm(2.)/float y.Count
let forecastlin      (beta:Generic.Vector<_>)  (x:Generic.Vector<_>) =  x.DotProduct(beta)
//estimator
let leastsquare (X:Generic.Matrix<_>)     (Y:Generic.Vector<_>)  = (X.Transpose()*X).Inverse() * X.Transpose().Multiply(Y)
let regLS       (lambda:float) (X:Generic.Matrix<_>)     (Y:Generic.Vector<_>)  = (X.Transpose()*X + lambda * Double.DenseMatrix.Identity(X.ColumnCount) ).Inverse() * X.Transpose().Multiply(Y)

let covar  = let a = Double.DenseMatrix.diag(Double.DenseVector( [|5.;1.;3.|])) 
             let c = Rotation [|-0.5*System.Math.PI/2.;0.3*System.Math.PI/2.|]  //0 : uncorrelated, pi/2 :100% correlated
             (a * c.R) * (a * c.R).Transpose()

let X     = Normal.generaten 100 covar |> DataMatrix
let beta  = Double.DenseVector( [|5.;1.;3.|])
let noise = 1.
let Y        = nApply (forecastlin beta >> addnoise noise) X


//Cross valid
let d = DataSet(X,Y)
let m = Model(leastsquare,forecastlin, rss)


let slices max nstep = [0. .. max /float nstep .. max ]
List.zip (slices 10. 20)(slices 10. 20 |> List.map (fun l -> let m = Model(regLS(l),forecastlin, rss) in m.kfold 5  d)) |> FSharpChart.Line


////base case
//if false then
//   empiricalVar X
//   let x = [1..1000]
//   List.zip x (x |> List.map (fun i -> let x = Normal.generaten i covar |> DataMatrix
//                                       (empiricalVar x).Determinant() )) |> FSharpChart.Line
//   let Yhat = X.Multiply(leastsquare X Y)
//   rss (Yhat - Y) 
//   () |> ignore
//
//if false then 
//   let test n f = 
//      let x = [2..n]
//      List.zip x (x |> List.map f) |> FSharpChart.Line
//   test 100 (fun i-> m.kfold i d)
//   m.kfold 500    d
//   m.structErr  d d
//   ()
//if false then 
//   //GRAPH
//   Normal.generaten 100 covar  |> List.map (fun v -> (v.[0], v.[1]))  |> FSharpChart.Point
//   Normal.generaten 100 covar  |> List.map (fun v -> (v.[0], v.[2]))  |> FSharpChart.Point
//   Normal.generaten 100 covar  |> List.map (fun v -> (v.[1], v.[2]))  |> FSharpChart.Point

//   let t = Double.Factorization.DenseSvd(svd, true)
//   let u, d, vt = t.U(), t.S(), t.VT()


//
//type Zero     = Zero with
//    static member        ( + ) (Zero  , b) = b
//    static member        ( * ) (Zero  , b) = Zero
// 
//type Succ<'a> = Succ of 'a with
//    static member inline ( + ) (Succ a, b) = Succ (a + b)
//    static member inline ( * ) (Succ a, b) = b + (a * b)
//
//let f (a:Succ<Zero>) b : Succ<Succ<Succ<Zero>>> = a + b   
//
//let rec ff n = match n with
//   | 0 -> Zero
//   | _ -> Succ(ff (n-1))
//
//[<Measure>] type s
//type M<[<Measure>]'w, [<Measure>] 'h>() = 
//    static member (*) (a : M<'a, 't>, b : M<'t, 'b>) : M<'a, 'b> = failwith "NYI"
//let x = M<s ^ 3, s ^ 3>()
//let y = M<s ^ 3, s>()
//let z  = x * y // M<s ^ 3, s> 
//
//
//
//
//let somx = generaten 90 covar |> Seq.toArray |> Array.map (fun v -> { position = new Vector(v.[0], v.[1]) })
////graph tests
//if false then
//   let rnd = System.Random()
//   let next ()= (rnd.Next(999) |> float) / 999.
//   let somx = Array.init 10 (fun _ -> { position = new Vector(next(), next()) })
//   spawn (fun () -> make_window(somx, Seq.empty)) () |> ignore
//
//if false then
//   let lambda = vector [1.;2.]
//   let sigma = lambda.OuterProduct lambda
//   let svd =  sigma :?> Double.DenseMatrix
//
//   let t = Double.Factorization.DenseSvd(svd, true)
//   let u, d, vt = t.U(), t.S(), t.VT()
//   d.ToArray() |> ignore







//
//
//
////
//let mv = Double.DenseVector(2).ToColumnMatrix() :?> Double.DenseMatrix
//let mm = Double.DenseMatrix.Identity(2)
//let ida =  Double.DenseMatrix.Identity(2)
//let idb =  Double.DenseMatrix.Identity(1)
//let m = mm
//let generator  = MatrixNormal(ida, ida, ida)
//let generator2 = MatrixNormal( mv, idb, ida)
//generator.Density(mm)
//generator2.Density(mv)
//
