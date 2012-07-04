namespace FStat.Distributions

open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open Microsoft.FSharp.Math.SI
open FStat.LinearAlgebra

module Normal = 
   let rnd = new MathNet.Numerics.Random.MersenneTwister()
   let log2pi = 1.837877066

   let sample = 
      fun () -> let rec randomNormal() = let u1, u2 = rnd.NextDouble(),rnd.NextDouble()
                                         let r, theta= sqrt (-2. * (log u1)), 2. * System.Math.PI * u2  
                                         seq { yield r * sin theta; yield r * cos theta ; yield! randomNormal() }
                randomNormal()
   let Next()   = sample() |> Seq.head
   let NextV(n) = DenseVector( sample() |> Seq.take n |> Seq.toArray)
   let generate (covar:Matrixu<'u^2>) = 
      let R = if covar.Determinant() = 0.<_> 
              then  let u, d, vt = covar.Svd()                                      //printfn "svd ok = %A  %A" (covar) ((u * d  * vt))             printfn "symetric ok = %A %A" (covar)( (u * d  * u.Transpose()))
                    let A = (d.map sqrt) * u.Transpose()                          //printfn "rd ok = %A %A" (d )( rd*rd)               printfn "covarrd ok = %A %A" (covar )( u*rd.Transpose()*rd*u.Transpose())
                    let _ , r = A.QR() in r.Transpose()                              //printfn "covar ok = %A %A" (covar)((q*r).Transpose()*(q*r))               printfn "covar2 ok = %A %A" (covar)(r.Transpose()*r)
               else let chol = covar.Cholesky()
                    chol.Factor
      let Ru = Matrixu<1> R
      fun () -> let v = Vectoru<'u>( sample() |> Seq.take(covar.ColumnCount))
                Ru * v
   let generaten n covar = 
         let generatecovar = generate covar
         List.init n (fun _ -> generatecovar ()) 

   //0 : uncorrelated, pi/2 :100% correlated
   let rotationtoR (thetas:float array) =
      let covar = DenseMatrix.Identity(1+thetas.Length)
      thetas |> Array.iteri (fun i theta -> seq { 0 .. i } |> Seq.iter(fun j -> covar.[1+i,j] <- covar.[i,j] * sin theta )
                                            covar.[1+i,1+i] <- cos theta )
      covar
   
   let covarDiagRot<[<Measure>]'u> (stddev:float array) rotations = 
         //let aa = Vectoru(stddev)
         let a = DenseMatrixU.diag(Vectoru<'u>(stddev))  //[|5.;1.;3.|]
         let c  = Matrixu<1> (rotationtoR rotations)
         (a * c) * (a * c).Transpose()


   let negLogLike (mu:float<'u>) (sigma:Matrixu<'u^2>)  =
      let precision = sigma.Inverse()
      fun (x:Vectoru<'u>) -> 0.5 * ((float x.Count)  * log2pi + log (float (sigma.Determinant())) + log ( (precision  * (x.OuterProduct(x))).Trace() )  )