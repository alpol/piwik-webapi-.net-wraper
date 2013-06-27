#I @"C:\Projects\Research\Piwik\piwik-webapi-.net-wraper\packages\FSharp.Data.1.1.1\lib\net40"
#r @"FSharp.Data.dll"

#load "PiwilApiParameter.fs"
#load "PiwikMethodDefs.fs"
#load "PiwikCallBuilder.fs"


open System
open FSharp.Data
open PiwikApiParameter
open PiwikMethodDefs
open PiwikCallBuilder

let t = System.IO.File.ReadAllLines(@"c:\TEMP\piwik-test.txt").[0].Split([|','|])
let serviceUri = t.[0]
let authToken = t.[1]

let getOne () =
    let startDay = DateTime.Now.AddDays -5.0
    let endDay  = DateTime.Now
    let apiCall =  serviceUri |> start |>  addAuth authToken |>  addFormat FormatType.Csv 
                    |> addMethod (PiwikMethodDefs.MultiSites (PiwikMethodDefs.MultiSitesMethod.GetOne( (SiteId.Single 2),(TimeSlice.Date(endDay,PeriodType.Week)),None,None)))
                    //|> addSegment (Segments([(VisitLocationCity(Equals,"Boston"),Or)], VisitLocationProvider(Equals,"comcast.net")))
                    //|> addParameter("label","veterans")
                    |> addParam(ExpandedType.Expanded)
                    |> addFilter (FilterType.Limit 1)
    
    execute apiCall   
    
let result:string = getOne()  
let path = @"C:\Temp\test.csv"
System.IO.File.WriteAllText(path,result)
type DataR = CsvProvider<"C:/Temp/test.csv"> 

let d = DataR.Parse(result)
let fr = d.Data |> Seq.head
d.Headers |> Seq.iter (fun e -> printfn "%A" e)
for r in d.Data  do
    System.Console.WriteLine(r.MetadataIdsite)
    System.Console.WriteLine()


Console.Read()|> ignore
