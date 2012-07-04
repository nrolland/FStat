namespace FStat.LinearAlgebra

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double
open Microsoft.FSharp.Math.SI
open Microsoft.FSharp.Core.LanguagePrimitives

type Vectoru<[<Measure>]'u>  =
   val v : Double.DenseVector 
   //inherit Double.DenseVector
   new (size:int)                    = { v =  Double.DenseVector(size) }
   new (arF :seq<float>)            = { v =  Double.DenseVector(arF |> Seq.map (float)  |> Seq.toArray)  }
   new (v   :Generic.Vector<float>)  = { v =  Double.DenseVector(v)  }
   //new (v   :Vectoru<'v>)           = { inherit Double.DenseVector(v)  }
   static member (-)(leftSide:Vectoru<'u>, rightSide:Vectoru<'u>) = Vectoru<'u>(leftSide.v.Subtract(rightSide.v))
   static member (+)(leftSide:Vectoru<'u>, rightSide:Vectoru<'u>) = Vectoru<'u>(leftSide.v.Add(rightSide.v))
   static member (*)(leftSide:Vectoru<'u>, alpha:float<'v>)       = Vectoru<'u*'v>( leftSide.v.Multiply(float alpha))
   static member (*)(alpha:float<'v>, rightSide:Vectoru<'u>)      = Vectoru<'u*'v>(rightSide.v.Multiply(float alpha))

   member this.Multiply(alpha:float<'v>)              = Vectoru<'u*'v>(this.v.Multiply(float alpha))
   member this.Norm(p:float):float<'u>                = this.v.Norm(p)         |> FloatWithMeasure
   member this.DotProduct(v:Vectoru<'v>):float<'u*'v> = this.v.DotProduct(v.v) |>  FloatWithMeasure 
   member this.Count                                  = this.v.Count
   member this.map (f:float<'u> -> float<'v>)         = Vectoru<'v>     (this.v.GetIndexedEnumerator()  |> Seq.map (snd >> FloatWithMeasure<'u> >> f >> float))
   member this.ToRowMatrix()                          = Matrixu<'u>     (this.v.ToRowMatrix())
   member this.OuterProduct(v:Vectoru<'v>)            = Matrixu<'u*'v>  (this.v.OuterProduct(v.v))
   member this.SubVector(index, length)               = Vectoru<'u>     (this.v.SubVector(index, length))
   member this.Item
         with get(key1) : float<'u> = this.v.[key1] |> FloatWithMeasure
         and  set (key1) (value:float<'u>) = this.v.[key1] <- float value
                         
and Matrixu<[<Measure>] 'u>  =
   val m : Double.DenseMatrix
   new (size:int)                               = { m = Double.DenseMatrix(size)        }
   new (sizei:int, sizej:int                  ) = { m = Double.DenseMatrix(sizei,sizej) }
   new (sizei:int, sizej:int, ar:float<'u> seq) = { m = Double.DenseMatrix(sizei,sizej, ar |> Seq.map (float)  |> Seq.toArray) }
   new (m:Generic.Matrix<float>)                = { m = Double.DenseMatrix(m.ToArray()) }

   static member Identity(order) = Matrixu<'u>(Double.DenseMatrix.Identity(order))
   static member (*)(leftSide:Matrixu<'u>, rightSide:Matrixu<'v>)    = Matrixu<'u 'v> (leftSide.m.Multiply(rightSide.m))
   static member (*)(leftSide:Matrixu<'u>, rightSide:Vectoru<'v>)    = leftSide.Multiply(rightSide)
   static member (*)(leftSide:float<'u>  , rightSide:Matrixu<'v>)    = Matrixu<'u 'v> ((float leftSide) * rightSide.m)
   static member (*)(leftSide:Matrixu<'u>, rightSide:float<'v>)      = Matrixu<'u 'v> ((float rightSide) * leftSide.m)
   static member (+)(leftSide:Matrixu<'u>, rightSide:Matrixu<'u>)    = Matrixu<'u>    (leftSide.m.Add(rightSide.m))
   static member (-)(leftSide:Matrixu<'u>, rightSide:Matrixu<'u>)    = Matrixu<'u>    (leftSide.m.Subtract(rightSide.m))
   static member (/)(leftSide:Matrixu<'u>, rightSide:float<'v>)      = Matrixu<'u/'v> (leftSide.m.Divide(float rightSide))
   static member (/)(leftSide:float<'v>  , rightSide:Matrixu<'u>)    = Matrixu<'v/'u> (rightSide.m.Divide(float leftSide))


   member this.RowEnumerator()            = let a = this.m.RowEnumerator() in  a |> Seq.map (fun (i, v) -> i, Vectoru<'u>(v))
   member this.Transpose()                = Matrixu<'u>   (this.m.Transpose())
   member this.Divide(a:float<'v>)        = Matrixu<'u/'v>(this.m.Divide(a))
   member this.Inverse()                  = Matrixu<'u^-1>(this.m.Inverse())
   member this.Multiply(v:Vectoru<'v>)    = Vectoru<'u*'v>(this.m.Multiply(v.v))
   member this.Diagonal()                 = Vectoru<'u>   (this.m.Diagonal())
   member this.Row(i)                     = Vectoru<'u>   (this.m.Row(i))
   member this.RowCount                   = this.m.RowCount
   member this.ColumnCount                = this.m.ColumnCount
   member this.Append(x:Matrixu<'u>)      = Matrixu<'u>   (this.m.Append(x.m))
   member this.Determinant():float<'u>    = this.m.Determinant()  |> FloatWithMeasure
   member this.Trace():float<'u>          = this.m.Trace()        |> FloatWithMeasure
   member this.Svd()                      = let t = this.m.Svd(true) in Matrixu<'u> (t.U()), Matrixu<'u> (t.W()), Matrixu<'u> (t.VT())
   member this.map(f:float<'u>->float<'v>)= Matrixu<'v>(this.RowCount, this.ColumnCount, this.m.IndexedEnumerator()  |> Seq.map((fun (i,j,v) -> v) >> FloatWithMeasure >> f))
   member this.SubMatrix(rowIndex,rowLength,columnIndex,columnLength) = Matrixu<'u>   (this.m.SubMatrix(rowIndex, rowLength, columnIndex, columnLength))

   member this.Item
         with get(key1, key2) : float<'u> = this.m.[key1, key2] |> FloatWithMeasure
         and  set (key1, key2) (value:float<'u>) = this.m.[key1, key2] <- float value


   //let private mapply (m:Matrix<'u>) f = m.IndexedEnumerator() |> Seq.iter(fun (i,j,v) -> m.[i,j] <- f v ); m


// A module which implements functional dense vector operations.
module DenseMatrixU =
    let inline diag (v: #Vectoru<'u>) =
        let n = v.Count
        let A = new Matrixu<'u>(n,n)
        for i=0 to n-1 do
            A.[i,i] <- v.Item(i)
        A

//    let toto =    
//      let one = [|1.|]
//      let a = diag(Vectoru<Microsoft.FSharp.Math.SI.m>(one))  //[|5.;1.;3.|]
//      let b = diag(Vectoru<Microsoft.FSharp.Math.SI.s>(one))  //[|5.;1.;3.|]
//      let c = a * b 
//      ()



//type A =
//   val x : int
//   val y : int
//   new(xv, yv) = { x = xv; y = yv }
//   new(yv) = A(0, yv)  // CSharp: this(0, yv)
//type B = 
//   inherit A
//   val a : int
//   new() = { inherit A(10, 20); a = 42 }
//
