#if INTERACTIVE
#load @"__solmerged.fsx" 
#endif
#if COMPILED
module Tests
#endif

open System
open FsCheck
open Prelude
open Xunit
open Swensen.Unquote

open FStat.Model
open FStat.Extensions
open FStat.Distributions
open FStat.LinearAlgebra


open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Generic
open MathNet.Numerics.LinearAlgebra.Double


type Approximate = Approximate with
    static member inline ($) (Approximate, x:^n       ) = fun (y:^n) -> float (abs (x-y)) <  1.E-10
    static member inline ($) (Approximate, x:list< ^n>) = 
        fun (y:list< ^n>) -> 
            x.Length = y.Length && (List.zip x y |> List.forall ( fun (a,b) -> (Approximate $ a) b))
let inline (=~=) x y = (Approximate $ x) y
let equalapprox (x:list<float<'u>>)  (y:list<float<'u>>) err = 
   x.Length = y.Length && (List.zip x y |> List.forall(fun(a,b)->float(abs(a-b))<err))
let equalapprox2  (x:float<'u>) (y:float<'u>)  err = float(abs(x-y))<err


let inline (=~.=) x y = float (abs x-y) <  1.E-10

let test3  = [1;2] =~= [1;2]

[<Fact>]
let ``should fail`` () =
    test <@ ([3; 2; 1; 0] |> List.map ((+) 1)) = [1 + 3..1 + 0] @>
[<Fact>]
let ``unquote - passing test `` () =
    test <@ ([0; 1; 2; 3] |> List.map ((+) 1)) = [1..4 ] @>

[<Fact>]
let ``covar from rotation `` () =
    test <@  let c = Normal.rotationtoR [|System.Math.PI/2.; System.Math.PI/2.|] 
             equalapprox [ c.[0,0]; c.[1,0] ; c.[2,0] ] [1.;1.;1.] 1.E-10 &&
             equalapprox [ c.[1,1]; c.[1,2] ; c.[2,1] ] [0.;0.;0.] 1.E-10 @>

[<Fact>] 
let testSS() = 
  let sumsquare  (y:Vector<float>)   =  pown (y.Norm(2.)) 2 
  let n = 100
  let v = Double.DenseVector( Normal.sample() |> Seq.take n |> Seq.toArray)   
  test <@ equalapprox2 (sumsquare v / float n) 1.  (1.96*2./sqrt(float n))  @> 
  //apply TCL : xi = mu,sigma2 // sum xi = n.mu,n.sigma2 // 1/n sum xi = mu,sigma2/n
  //X2 = 1, 2 
 
  
[<Fact>] 
let testRegression() = 

   let n = 100
   let covar       = Normal.covarDiagRot<regressor>[|1.;1.;1.|] [|-0.5*Math.PI/2.;0.3*Math.PI/2.|]

   let X           = Normal.generaten n covar
   let beta, noise = Vectoru([|5.;2.;3.|]), 1.<output>
   let Y           = (nApply (LinearModel beta >> addnoise noise)) X

   let m           = Model(LinearModel, sumsquare >> float, LS)
   let betahat     = m.fit (DataSet(X,Y))

   test <@  equalapprox2  betahat.[1] beta.[0]  (1.96/sqrt(float n))  && 
            equalapprox2  betahat.[2] beta.[1]  (1.96/sqrt(float n))  && 
            equalapprox2  betahat.[3] beta.[2]  (1.96/sqrt(float n))  
        @>




let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs 

[<Fact>] 
let test_prop_average() = 
  Check.Quick revRevIsOrig



[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args
    0



//let testroot = @"C:\Users\e021230\Documents\Visual Studio 11\Projects\divsharp\"
//let testrel =  "..\\..\\..\\.\\..\\Documents\\Visual Studio 11\\Projects\\divsharp\\Financial\\office.dll"
//let testres = toabsolute testroot  testrel  // "C:\Users\e021230\Documents\Visual Studio 11\Projects\divsharp\Financial\office.dll"
//let absfile = DirectoryInfo(testroot + testrel).Parent.FullName
//let difile = DirectoryInfo("C:\Users\e021230\Documents\Visual Studio 11\Projects\office.dll")
//let diroot = DirectoryInfo(testroot)
//let difile1 = DirectoryInfo @"C:\Users\e021230\Documents\Visual Studio 11\"
//let difile2 = DirectoryInfo testrel
//let test = difile
//
//difile2.FullName = DirectoryInfo(diroot.FullName + "\\" + writerelative diroot difile2 ).FullName
//writerelative diroot difile
//writerelative diroot difile1
//writerelative diroot difile2


