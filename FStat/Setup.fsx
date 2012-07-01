#r "System.Xml.Linq"

open System
open System.IO
open System.Xml.Linq
open System.Text.RegularExpressions


let modifythis = @"\\psf\Home\Documents\pc\FStat\"
let root = modifythis.TrimEnd('\\')

//It might need some manual cleaning of the resulting files but gets the job done

let refsforaproject dirproject = 
   seq {
         //TODO: this currently loads fsproj's in alphabeticall order, we should instead
         //build the dependencies graph of the fsproj's and load them in topological sort order
         for fsProjFile in Directory.GetFiles(dirproject, "*.fsproj") do
                 let getElemName name = XName.Get(name, "http://schemas.microsoft.com/developer/msbuild/2003")
                 let getElemValue name (parent:XElement) =
                     let elem = parent.Element(getElemName name)
                     if elem = null || String.IsNullOrEmpty elem.Value then None else Some(elem.Value)

                 let getAttrValue name (elem:XElement) =
                     let attr = elem.Attribute(XName.Get name)
                     if attr = null || String.IsNullOrEmpty attr.Value then None else Some(attr.Value)

                 let (|??) (option1: 'a Option) option2 =
                     if option1.IsSome then option1 else option2

                 let fsProjFile = Directory.GetFiles(dirproject, "*.fsproj") |> Seq.head
                 let fsProjXml = XDocument.Load fsProjFile

                 let refspath = 
                     fsProjXml.Document.Descendants(getElemName "Reference")
                     |> Seq.choose (fun elem -> getElemValue "HintPath" elem)
                     |> Seq.map (fun ref -> ("#r ", false, ref)) //.Replace(@"\", @"\\")

                 let refsgac = 
                     fsProjXml.Document.Descendants(getElemName "Reference")
                     |> Seq.choose (fun elem -> if (getElemValue "HintPath" elem).IsNone then getAttrValue "Include" elem else None)
                     |> Seq.map (fun ref -> ("#r ", true, ref))

                 let fsFiles = 
                     fsProjXml.Document.Descendants(getElemName "Compile")
                     |> Seq.choose (fun elem -> getAttrValue "Include" elem)
                     |> Seq.map (fun fsFile -> ("#load ", false  ,  ".\\" + fsFile))

                 let projDll = 
                     fsProjXml.Document.Descendants(getElemName "ProjectReference")
                     |> Seq.choose (fun elem -> getAttrValue "Include" elem)
                     |> Seq.map (fun fsFile ->  let proj = (fsFile.Split ([|'\\'|])  |> Seq.last).Split ([|'.'|])  |> Seq.head
                                                ("#r " , true, root + "\\" + proj +   "\\bin\\Debug\\" + proj + ".dll"))
                 yield! refspath
                 yield! refsgac
                 yield! projDll
                 yield! fsFiles
   }


let (|Startwith2dots|Startwith1dot|Other|) (str: string) = // printfn "%A" str
                                                           if   Regex.IsMatch(str, "^(\.\.\\\\)")  then Startwith2dots 
                                                           elif Regex.IsMatch(str, "^(\.\\\\)")    then Startwith1dot
                                                           else Other

let minusonlevel (abs:string) = let trim =(abs.TrimEnd('\\'))
                                let r = trim.Split('\\') |> Array.rev |> Seq.skip 1 |> Seq.toArray |> Array.rev 
                                let abs'= r |> Seq.skip 1   |> Seq.fold (fun r  s -> r + "\\" + s )  (r |> Seq.head)
                                abs'.TrimEnd('\\')

let rec mergerel abs (rel:string) = match rel with | Startwith2dots ->  mergerel (minusonlevel abs) (rel.Substring(3))
                                                   | Startwith1dot -> mergerel abs (rel.Substring(2))
                                                   | Other -> //printfn "%A" rel
                                                              abs + "\\"  + rel 


let testroot = @"C:\Users\e021230\Documents\Visual Studio 11\Projects\divsharp\"
let testrel =  "..\\..\\..\\.\\..\\Documents\\Visual Studio 11\\Projects\\divsharp\\Financial\\office.dll"
let testres = mergerel testroot  testrel  // "C:\Users\e021230\Documents\Visual Studio 11\Projects\divsharp\Financial\office.dll"


let getprojectdir root = 
   let rec getdirs root = seq {
      yield! Directory.GetDirectories(root) |> Array.filter(fun f -> Directory.GetFiles(f, "*.fsproj") |> Array.length > 0 ) 
      yield! Directory.GetDirectories(root) |> Array.map(fun d -> getdirs d) |> Seq.concat}
   
   getdirs root   

for fsProj in getprojectdir root do
   let projname = fsProj.Split ([|'\\'|])  |> Seq.last
   let refs = refsforaproject fsProj |> Seq.map (fun (cmd,isabs, relpath) ->  cmd, if isabs then relpath else mergerel (fsProj)  relpath)
                                     |> Seq.map (fun (cmd, abspath) ->  cmd +  "@\""  + abspath +  "\"")
   let tempFile = Path.Combine(fsProj, "__" + (fsProj.Split ([|'\\'|])  |> Seq.last)  + ".fsx")
   File.WriteAllLines(tempFile, refs)    


let finalFileContent  =  seq { for fsProj in getprojectdir root do
                                 let name = Path.Combine(fsProj, "__" + (fsProj.Split ([|'\\'|])  |> Seq.last)) + ".fsx"
                                 yield "#load @\"" +  name  + "\"" }
let tempFile = Path.Combine(__SOURCE_DIRECTORY__, "__sol.fsx")
File.WriteAllLines(tempFile, finalFileContent)    

let merged root  = seq { 
   for fsProj in getprojectdir root do
      let projname = fsProj.Split ([|'\\'|])  |> Seq.last
      yield! refsforaproject fsProj |> Seq.filter (fun (cmd,isabs, relpath) ->  cmd <> "#load ")
                                    |> Seq.map    (fun (cmd,isabs, relpath) ->  cmd, if isabs then relpath else (mergerel (root + "\\" + projname )  relpath))
                                    |> Seq.map    (fun (cmd, abspath) ->  cmd +  "@\""  + abspath +  "\"")
      }

let mergedFile = Path.Combine(__SOURCE_DIRECTORY__, "__solmerged.fsx")
File.WriteAllLines(mergedFile, merged root |> Set.ofSeq)    
