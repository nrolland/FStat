namespace FStat.Model

open FStat.Extensions
open MathNet.Numerics.LinearAlgebra.Generic


type DataSet  = DataSet of Matrix<float> * Vector<float>
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

