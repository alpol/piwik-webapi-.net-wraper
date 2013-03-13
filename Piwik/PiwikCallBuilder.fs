module PiwikCallBuilder

open PiwikApiParameter
open PiwikMethodDefs
open System

let start (uri:string) =
    if uri.Contains("?module=API") then raise(ParameterDuplicationException("module", "API")) 
    if(uri.EndsWith(@"/")) then
        String.Format(@"{0}?module=API", uri)
    else
        String.Format(@"{0}/?module=API", uri)

let addAuth (token:string) (uri:string) = uri |> addParam (AuthToken token) 

let addLanguage (language:string) (uri:string) = uri |> addParam (Language language)

let addLable (lable:string) (uri:string) = uri |> addParam (Lable lable)

let addSite (sites:SiteId) (uri:string) = uri |> addParam sites

let addPeriod (pType:TimeSlice)  (uri:string) = uri |> addParam pType
 
let addFormat (format:FormatType) (uri:string) = uri |> addParam format

let addMethod (methodName:PiwikMethodDefs.MethodType) (uri:string) = uri |> addParam methodName

let addSegment (segment:SegmentType) (uri:string) = uri |> addParam segment

let addFilter (filter:FilterType) (uri:string) = uri |> addParam filter

let addSort (sort:SortType) (uri:string) = uri |> addParam sort

let disableGenericFilters (uri:string) = addParameter ("disable_generic_filters","1") uri

let disableQueuedFilters (uri:string) = addParameter ("disable_queued_filters","1") uri

let addIdSubtable (id:int)(uri:string) = addParameter("idSubtable", id.ToString()) uri 

let addEnhanced (uri:string) = addParameter("enhanced", "true") uri 

let execute (apiCall:string) =
    let webCl = new System.Net.WebClient()
    webCl.DownloadString(apiCall)
