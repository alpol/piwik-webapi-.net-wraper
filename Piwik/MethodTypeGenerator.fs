module MethodTypeGenerator
#if INTERACTIVE  
    #r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Xml.Linq.dll"
#endif

open System
open System.Linq
open System.Xml
open System.Xml.Linq

let methodNameCommandDef = @"    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)"

let moduleNameMathDef = "| MODULENAMEPLACEHOLDER(n) -> String.Format(\"&{0}={1}\", (this :> ApiParameter).Name, \"MODULENAMEPLACEHOLDER.\" + n.Command ) "

let sourcePath = @"C:\Projects\LCLC\DataLoad\Piwik\piwik.xml"
let source = XDocument.Load(sourcePath)
let moduleNameDefs = new System.Collections.Generic.List<(string*string)>()
let methodNameDefs = new System.Collections.Generic.List<string>()

let methodNameDef = new System.Text.StringBuilder()

let mutable moduleName = ""
let addModuleNewModule (name:string) =
    let m = String.Format(@"|{0} of {0}Method",name)
    let mt = moduleNameMathDef.Replace("MODULENAMEPLACEHOLDER",name)
    moduleNameDefs.Add(m,mt)
    
let addMethodNewMethodHeader (moduleName:string) (methodName:string)=
    ignore(methodNameDef.Append(methodNameCommandDef))
    methodNameDefs.Add(methodNameDef.ToString())
    ignore(methodNameDef.Clear())
    ignore(methodNameDef.AppendLine(String.Format( "and {0}Method =", moduleName)))
    ignore(methodNameDef.AppendLine(String.Format(@"    | {0}", methodName.Substring(0,1).ToUpper() + methodName.Substring(1))))


let processRow (xElm:XElement) =
    let mutable processingModule = (xElm.Descendants (XName.Get "module") |> Seq.exactlyOne).Value
    let mutable processingMethod = (xElm.Descendants(XName.Get "action") |> Seq.exactlyOne).Value
    if moduleName <> processingModule then
        moduleName <- processingModule
        addModuleNewModule moduleName
        addMethodNewMethodHeader moduleName processingMethod
    else
        ignore(methodNameDef.AppendLine(String.Format(@"    | {0}", processingMethod.Substring(0,1).ToUpper() + processingMethod.Substring(1))))
    

source.Root.Descendants(XName.Get "row") |> Seq.length
source.Descendants (XName.Get"row") |> Seq.iter (fun e -> processRow e)

ignore(methodNameDef.Append(methodNameCommandDef))
methodNameDefs.Add(methodNameDef.ToString())

moduleNameDefs |> Seq.iter (fun t -> printfn "%s" (fst t) )
moduleNameDefs |> Seq.iter (fun t -> printfn "%s"  (snd t))
methodNameDefs |> Seq.iter (fun t -> printfn "%s" t)


