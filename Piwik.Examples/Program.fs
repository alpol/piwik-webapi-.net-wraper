open System
open FSharp.Data
open PiwikApiParameter
open PiwikMethodDefs
open PiwikCallBuilder

let getUrlAndToken () =
    let t = System.IO.File.ReadAllLines(@"c:\TEMP\piwik-test.txt").[0].Split([|','|])
    t.[0],t.[1]

let getOne () =
    let startDay = DateTime.Now.AddDays -5.0
    let endDay  = DateTime.Now
    let serviceUri,authToken = getUrlAndToken()
    let apiCall =  serviceUri |> start |>  addAuth authToken |>  addFormat FormatType.Xml |> addPeriod (TimeSlice.Date(endDay,PeriodType.Day))
                    |> addMethod (PiwikMethodDefs.MultiSites (PiwikMethodDefs.MultiSitesMethod.GetOne( (SiteId.Single 2),(TimeSlice.Today),None,None)))
                    //|> addSegment (Segments([(VisitLocationCity(Equals,"Boston"),Or)], VisitLocationProvider(Equals,"comcast.net")))
                    //|> addParameter("label","veterans")
                    |> addParam(ExpandedType.Expanded)
                    |> addFilter (FilterType.Limit 1)
    Console.WriteLine apiCall
    execute apiCall


   
[<EntryPoint>]
let main argv = 
    getOne() |> Console.Write
    Console.ReadLine() |> ignore
    0 // return an integer exit code
