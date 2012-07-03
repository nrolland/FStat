namespace FStat.LinearAlgebra

//open MathNet.Numerics.LinearAlgebra.Double
//open MathNet.Numerics.LinearAlgebra
//open MathNet.Numerics.FSharp
////open Microsoft.FSharp.Math.SI
//open System.Globalization
//open System.Collections.Generic
//open System.Linq
//open System
open MathNet.Numerics.LinearAlgebra


type A =
   val x : int
   val y : int
   new(xv, yv) = { x = xv; y = yv }
   new(yv) = A(0, yv)  // CSharp: this(0, yv)
type B = 
   inherit A
   val a : int
   new() = { inherit A(10, 20); a = 42 }

type Vectoru<[<Measure>] 'u>  =
   inherit Double.DenseVector
   new (size:int)                   = { inherit Double.DenseVector(size) }
   new (arF :float<'u> array)       = { inherit Double.DenseVector(arF |> Array.map (float))  }
   new (v   :Generic.Vector<float>) = { inherit Double.DenseVector(v)  }
                         
type Matrixu<[<Measure>] 'cu, [<Measure>] 'lu>  =
   inherit Double.DenseMatrix
   new (size:int)              = { inherit Double.DenseMatrix(size) }
   new (sizei:int, sizej:int)  = { inherit Double.DenseMatrix(sizei,sizej) }
   new (m:Generic.Matrix<float>)  = { inherit Double.DenseMatrix(m.ToArray()) }

   static member Identity(order) = Matrixu<'cu, 'lu>(Double.DenseMatrix.Identity(order))
   static member (*)(leftSide:Matrixu<'cu,'lu>, rightSide:Matrixu<'lu,'lv>) =
         let a = leftSide.Multiply(rightSide)
         new Matrixu<'cu,'lv>(a)
   member this.RowEnumerator() = 
         let a = base.RowEnumerator()
         a |> Seq.map (fun (i, v) -> i, Vectoru<'lu>(v))
         
// A module which implements functional dense vector operations.
module DenseMatrixU =
    let inline diag (v: #Vectoru<'u>) =
        let n = v.Count
        let A = new Matrixu<'u,'u>(n,n)
        for i=0 to n-1 do
            A.[i,i] <- v.Item(i)
        A

    let toto =    
      let meters, seconds = [|1.<Microsoft.FSharp.Math.SI.m>|], [|1.<Microsoft.FSharp.Math.SI.s>|]
      let a = diag(Vectoru(meters))  //[|5.;1.;3.|]
      let b = diag(Vectoru(seconds))  //[|5.;1.;3.|]
      let c = a * b 
      ()


