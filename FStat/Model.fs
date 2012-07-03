namespace FStat.Model

open FStat.Extensions
open FStat.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double


type DataSet<[<Measure>] 'x, [<Measure>] 'y>  = DataSet of Matrixu<1,'x> * Vectoru<'y>
   with static member (+) (a:DataSet<'y,'x>,b:DataSet<'y,'x>):DataSet<'x,'y>  = 
            let (DataSet(x,y)), (DataSet(u,v)) = a, b
            let xx =    x.Transpose().Append(u.Transpose()).Transpose() 
            let yy = (y.ToRowMatrix().Append(v.ToRowMatrix())).Row(0)
            DataSet(Matrixu<1,'x>(xx),Vectoru<'y>(yy))

        static member merge (ar:DataSet<_,_> seq) : DataSet<_,_>  = 
            let   head = ar |> Seq.head
            let others = ar |> Seq.skip 1
            others |> Seq.fold (fun st el -> st + el) head

        member this.Splitat n = 
            let (DataSet(x,y)) = this
            DataSet(Matrixu<1,'x>(x.SubMatrix(0         ,n,0,x.ColumnCount)), Vectoru<'y>(y.SubVector(0, n         ))),
            DataSet(Matrixu<1,'x>(x.SubMatrix(n,x.RowCount,0,x.ColumnCount)), Vectoru<'y>(y.SubVector(n, x.RowCount)))

        member this.Splitin k = 
            let (DataSet(x,y)) = this
            let step, rem = (x.RowCount) / k, (x.RowCount- (x.RowCount) / k * k)
            seq { for i in seq {0 ..  k-1 } ->
                     let s, e = i * step, step  + if i = k-1 then rem else 0
                     DataSet(Matrixu<1,'x>(x.SubMatrix(s,e,0,x.ColumnCount)), Vectoru<'y>(y.SubVector(s,e))) }
        member this.DrawCount =
            let (DataSet(x,y)) = this
            x.RowCount

type Model (forecast:Vectoru<'p>->Vectoru<'x>-> float<'y>, loss:Vectoru<'y>-> float, estimationparam) =
   let defaultArg v = function | Some(v) -> v | _ -> v
   let augmentwone (x:Matrixu<1,'x>) = Matrixu<1,'x>(DenseMatrix(x.RowCount,1,1.).Append(x))

   member this.structErr(train, ?test, ?constantIncluded) =
      let (DataSet(xtrain,ytrain)), (DataSet(xtest,ytest)) = train, defaultArg train test
      let cxtrain, cxtest = if defaultArg false constantIncluded then xtrain,xtest else augmentwone xtrain, augmentwone xtest
      
      let param = estimationparam cxtrain ytrain
      let f = nApply (forecast param)// cxtest
      //let yhat = nApply (forecast param)  cxtest
      loss(ytest - yhat)


   member this.kfold (k:int) (dataset:DataSet) = 
      if k <= 1 then failwith "number of parts must be > 1"
      let artests, artrains = let ds = dataset.Splitin k in 
                              ds, let nds = ds |> Seq.mapi(fun i it -> (i,it)) in
                                  nds |> Seq.mapi (fun i s ->  DataSet.merge (nds |> Seq.filter(fun (j,s) -> not (i = j))|> Seq.map (snd))) 
   

      let results = Seq.zip artrains artests |> Seq.map(fun (train, test) -> //printfn "Train set %A \n Test set %A " train test 
                                                                             this.structErr(train,test))
      results |> Seq.average

   member this.fit  (DataSet(xtrain,ytrain) as d) = 
      estimationparam (augmentwone xtrain) ytrain

