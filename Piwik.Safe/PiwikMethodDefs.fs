module PiwikMethodDefs
open System
open PiwikApiParameter

type MethodType =
    |MultiSites of MultiSitesMethod
    |VisitsSummary of VisitsSummaryMethod
    |VisitTime of VisitTimeMethod
    |Actions of ActionsMethod
    |Referers of ReferersMethod
    |Goals of GoalsMethod
    |UserCountry of UserCountryMethod
    |CustomVariables of CustomVariablesMethod
    |VisitorInterest of VisitorInterestMethod
    |VisitFrequency of VisitFrequencyMethod
    |Provider of ProviderMethod
    |UserSettings of UserSettingsMethod
    |API of APIMethod
    interface ApiParameter with
        member this.Command =
            match this with
            | MultiSites(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "MultiSites." + n.Command ) 
            | VisitsSummary(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "VisitsSummary." + n.Command ) 
            | VisitTime(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "VisitTime." + n.Command ) 
            | Actions(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "Actions." + n.Command ) 
            | Referers(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "Referers." + n.Command ) 
            | Goals(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "Goals." + n.Command ) 
            | UserCountry(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "UserCountry." + n.Command ) 
            | CustomVariables(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "CustomVariables." + n.Command ) 
            | VisitorInterest(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "VisitorInterest." + n.Command ) 
            | VisitFrequency(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "VisitFrequency." + n.Command ) 
            | Provider(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "Provider." + n.Command ) 
            | UserSettings(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "UserSettings." + n.Command ) 
            | API(n) -> String.Format("&{0}={1}", (this :> ApiParameter).Name, "API." + n.Command ) 
        member this.Name = "method"
and MultiSitesMethod =
    | GetOne
    | GetAll
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and VisitsSummaryMethod =
    | Get
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and VisitTimeMethod =
    | GetVisitInformationPerServerTime
    | GetVisitInformationPerLocalTime
    | GetByDayOfWeek
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and ActionsMethod =
    | Get
    | GetPageUrls
    | GetEntryPageUrls
    | GetExitPageUrls
    | GetPageTitles
    | GetEntryPageTitles
    | GetExitPageTitles
    | GetOutlinks
    | GetDownloads
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and ReferersMethod =
    | GetRefererType
    | GetKeywords
    | GetWebsites
    | GetSearchEngines
    | GetCampaigns
    | GetSocials
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and GoalsMethod =
    | Get
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and UserCountryMethod =
    | GetCountry
    | GetContinent
    | GetRegion
    | GetCity
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and CustomVariablesMethod =
    | GetCustomVariables
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and VisitorInterestMethod =
    | GetNumberOfVisitsPerVisitDuration
    | GetNumberOfVisitsPerPage
    | GetNumberOfVisitsByVisitCount
    | GetNumberOfVisitsByDaysSinceLast
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and VisitFrequencyMethod =
    | Get
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and ProviderMethod =
    | GetProvider
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and UserSettingsMethod =
    | GetResolution
    | GetBrowser
    | GetBrowserVersion
    | GetBrowserType
    | GetPlugin
    | GetWideScreen
    | GetOS
    | GetConfiguration
    | GetOSFamily
    | GetMobileVsDesktop
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
and APIMethod =
    | Get
    member this.Command =
            match this with
            | _ as t -> (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)