#if INTERACTIVE
#load @"__solmerged.fsx" 
#endif
#if COMPILED
module Tests
#endif

open Xunit
open Swensen.Unquote
open FStat.Distributions
open FsCheck
open FsCheck.GenOperators
open FsCheck.Arb
open FsCheck.Commands
open FsCheck.Fluent
open FsCheck.Gen
open FsCheck.GenBuilder
open FsCheck.Prop
open FsCheck.PropOperators
open FsCheck.Random
open FsCheck.Runner
open Prelude

type Approximate = Approximate with
    static member inline ($) (Approximate, x:^n       ) = fun (y:^n) -> float (abs (x-y)) <  1.E-10
    static member inline ($) (Approximate, x:list< ^n>) = 
        fun (y:list< ^n>) -> 
            x.Length = y.Length && (List.zip x y |> List.forall ( fun (a,b) -> (Approximate $ a) b))
let inline (=~=) x y = (Approximate $ x) y

let inline (=~.=) x y = float (abs x-y) <  1.E-10

let test3  = [1;2] =~= [1;2]

[<Fact>]
let ``unquote - demo Unquote xUnit support`` () =
    test <@ ([3; 2; 1; 0] |> List.map ((+) 1)) = [1 + 3..1 + 0] @>
[<Fact>]
let ``unquote - passing test `` () =
    test <@ ([0; 1; 2; 3] |> List.map ((+) 1)) = [1..4 ] @>

[<Fact>]
let ``covar from rotation `` () =
    test <@  let c = Normal.rotationtoR [|System.Math.PI/2.; System.Math.PI/2.|] 
             [ c.[0,0]; c.[0,1] ; c.[0,2] ; c.[1,2] ] =~= [1.;1.;1.;1.]  @>

[<Fact>]
let ``unquote`` () =
    test <@  let c = Normal.rotationtoR [|System.Math.PI/2.; System.Math.PI/2.|] 
             1 =~.= 1 @>


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


