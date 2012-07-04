namespace FStat.Model

open FStat.Extensions
open FStat.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double


type DataSet<[<Measure>] 'x, [<Measure>] 'y>  = DataSet of Matrixu<'x> * Vectoru<'y>
   with static member (+) (a:DataSet<'x,'y>,b:DataSet<'x,'y>):DataSet<'x,'y>  = 
            let (DataSet(x,y)), (DataSet(u,v)) = a, b
            let xx =  x.Transpose().Append(u.Transpose()).Transpose() 
            let yy = (y.ToRowMatrix().Append(v.ToRowMatrix())).Row(0)
            DataSet(xx,yy)

        static member merge (ar:DataSet<'x,'y> seq) : DataSet<'x,'y>  = 
            let   head = ar |> Seq.head
            let others = ar |> Seq.skip 1
            others |> Seq.fold (fun st el -> st + el) head

        member this.Splitat n = 
            let (DataSet(x,y)) = this
            DataSet((x.SubMatrix(0         ,n,0,x.ColumnCount)), y.SubVector(0, n         )),
            DataSet((x.SubMatrix(n,x.RowCount,0,x.ColumnCount)), y.SubVector(n, x.RowCount))

        member this.Splitin k = 
            let (DataSet(x,y)) = this
            let step, rem = (x.RowCount) / k, (x.RowCount- (x.RowCount) / k * k)
            seq { for i in seq {0 ..  k-1 } ->
                     let s, e = i * step, step  + if i = k-1 then rem else 0
                     DataSet(x.SubMatrix(s,e,0,x.ColumnCount), y.SubVector(s,e)) }
        member this.DrawCount =
            let (DataSet(x,y)) = this
            x.RowCount

type Model<[<Measure>] 'p, [<Measure>] 'x, [<Measure>] 'y> 
      (forecast:Vectoru<'p>->Vectoru<'x>-> float<'y>, loss:Vectoru<'y>-> float<_>, estimationparam:Matrixu<'x>->Vectoru<'y>->Vectoru<'p>) =
   let defaultArg v = function | Some(v) -> v | _ -> v
   let augmentwone (x:Matrixu<'x>) = Matrixu<'x>(DenseMatrix(x.RowCount,1,1.).Append(x))

   member this.structErr(train:DataSet<'x,'y>, ?test, ?constantIncluded) =
      let (DataSet(xtrain,ytrain)), (DataSet(xtest,ytest)) = train, defaultArg train test
      let cxtrain, cxtest = if defaultArg false constantIncluded then xtrain,xtest else augmentwone xtrain, augmentwone xtest

      let a = cxtrain
      let b = ytrain
      let c = estimationparam
      let param = estimationparam cxtrain b
      let matrixforecastf = nApply (forecast param)// cxtest
      let yhat = matrixforecastf  cxtest
      let d = (ytest - yhat)
      loss(d)



   member this.kfold (k:int) (dataset:DataSet<_,_>) = 
      if k <= 1 then failwith "number of parts must be > 1"
      let artests, artrains = let ds = dataset.Splitin k in 
                              ds, let nds = ds |> Seq.mapi(fun i it -> (i,it)) in
                                  nds |> Seq.mapi (fun i s ->  DataSet.merge (nds |> Seq.filter(fun (j,s) -> not (i = j))|> Seq.map (snd))) 
   

      let results = Seq.zip artrains artests |> Seq.map(fun (train, test) -> //printfn "Train set %A \n Test set %A " train test 
                                                                             this.structErr(train,test))
      results |> Seq.average

   member this.fit  (DataSet(xtrain,ytrain) as d) = 
      estimationparam (augmentwone xtrain) ytrain

