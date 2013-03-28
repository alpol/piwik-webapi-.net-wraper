#I @"C:\Projects\Research\Piwik\piwik-webapi-.net-wraper\packages\FSharp.Data.1.1.1\lib\net40"
#r @"FSharp.Data.dll"

#load "C:\Projects\Research\Piwik\piwik-webapi-.net-wraper\Piwik\PiwilApiParameter.fs"
#load "PiwikMethodDefs.fs"
#load "C:\Projects\Research\Piwik\piwik-webapi-.net-wraper\Piwik\PiwikCallBuilder.fs"


open System
open FSharp.Data
open PiwikApiParameter
open PiwikMethodDefs
open PiwikCallBuilder

let t = System.IO.File.ReadAllLines(@"c:\TEMP\piwik-test.txt").[0].Split([|','|])
let serviceUri = t.[0]
let authToken = t.[1]
let startDay = DateTime.Now.AddDays -5.0
let endDay  = DateTime.Now

let getAll () =
    let apiCall =  serviceUri |> start |>  addAuth authToken |>  addFormat FormatType.Csv |> addPeriod (TimeSlice.Date(endDay,PeriodType.Day))
                    |> addMethod (MultiSites (GetAll))
                    //|> addSegment (Segments([(VisitLocationCity(Equals,"Boston"),Or)], VisitLocationProvider(Equals,"comcast.net")))
                    //|> addParameter("label","veterans")
                    |> addParam(ExpandedType.Expanded)
                    |> addFilter (FilterType.Limit 1)
    printfn "%s" apiCall
    execute apiCall   

    
let result = getAll()
let path = @"C:\Temp\test.csv"
System.IO.File.WriteAllText(path,result)  

type DataR = CsvProvider<"C:/Temp/test.csv"> 

let d = DataR.Parse(result)
let fr = d.Data |> Seq.head
d.Headers |> Seq.iter (fun e -> printfn "%A" e)
for r in d.Data  do
    System.Console.WriteLine("{0}-{1}-{2}-{3}",r.Label,r.MetadataIdsite,r.NbActions,r.NbVisits)
    System.Console.WriteLine()


Console.Read()|> ignore
