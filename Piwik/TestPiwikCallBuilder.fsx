#I @"C:\Projects\Research\Piwik\packages\FSharp.Data.1.1.1\lib\net40"
#r @"FSharp.Data.dll"

#load "PiwilApiParameter.fs"
#load "PiwikMethodDefs.fs"
#load "PiwikCallBuilder.fs"
#load "PiwikSafeMethodDefs.fs"

open System
open FSharp.Data
open PiwikApiParameter
open PiwikMethodDefs
open PiwikCallBuilder

let serviceUri = @""
let authToken = @""

let test () =
    let startDay = DateTime.Now.AddDays -5.0
    let endDay  = DateTime.Now
    let apiCall =  serviceUri |> start |>  addAuth authToken |>  addFormat FormatType.Csv |> addPeriod (TimeSlice.Date(endDay,PeriodType.Day))
                    |> addMethod (PiwikMethodDefs.MultiSites (PiwikMethodDefs.MultiSitesMethod.GetOne( (SiteId.Single 2),(TimeSlice.Date(endDay,PeriodType.Week)))))
                    //|> addSegment (Segments([(VisitLocationCity(Equals,"Boston"),Or)], VisitLocationProvider(Equals,"comcast.net")))
                    
                    //|> addParameter("label","veterans")
                    |> addParam(ExpandedType.Expanded)
                    //|> addFilter (FilterType.Limit 1)
    
    execute apiCall   
    
let result:string = test()  
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
