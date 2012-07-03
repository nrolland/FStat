namespace FStat.Distributions

open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic
open FStat.LinearAlgebra
open FStat.LinearAlgebra.DenseUnits
open Microsoft.FSharp.Math.SI

module Normal = 
   let rnd = new MathNet.Numerics.Random.MersenneTwister()
   let sampleNormal = 
      fun () -> let rec randomNormal() = let u1, u2 = rnd.NextDouble(),rnd.NextDouble()
                                         let r, theta= sqrt (-2. * (log u1)), 2. * System.Math.PI * u2  
                                         seq { yield r * sin theta; yield r * cos theta ; yield! randomNormal() }
                randomNormal()
   let Next() = sampleNormal() |> Seq.head
   let private mapply (m:Matrix<float>) f = m.IndexedEnumerator() |> Seq.iter(fun (i,j,v) -> m.[i,j] <- f v ); m
   let generate (covar:Matrix<float>) = 
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

   //0 : uncorrelated, pi/2 :100% correlated
   let rotationtoR (thetas:float array) =
      let covar = DenseMatrix.Identity(1+thetas.Length)
      thetas |> Array.iteri (fun i theta -> seq { 0 .. i } |> Seq.iter(fun j -> covar.[1+i,j] <- covar.[i,j] * sin theta )
                                            covar.[1+i,1+i] <- cos theta )
      covar
   
   let covarFromDiagAndRotation (stddev:float<_> array) rotations = 
         let a = DenseMatrixU.diag(Vectoru(stddev))  //[|5.;1.;3.|]
         let ar = [|1.<m>|]
         let b = DenseMatrixU.diag(Vectoru(ar))  //[|5.;1.;3.|]
         let d = a * b 
         let c = rotationtoR rotations                   //[|-0.5*System.Math.PI/2.;0.3*System.Math.PI/2.|]
         (a * c) * (a * c).Transpose()
