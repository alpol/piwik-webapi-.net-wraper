module PiwikMethodDefs

open PiwikApiParameter
open System

let private makeParam2 p = (p:>ApiParameter).Command 
let private makeParams p = p |> Seq.fold (fun s e -> s + (makeParam2 e) ) ""
let private makeParam (n, v) = String.Format(@"&{0}={1}",  n, v) 
let private makeParams2 p = p |> Seq.fold(fun s e -> s + (makeParam e))""
let private methodCmdImpl t = (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
let private methodTypeCmdImpl o m s = String.Format("&{0}={1}", (o :> ApiParameter).Name, s + "." + (m :> ApiMethod).Command ) 
let private getUNIXTimeStamp (dt:DateTime) = (dt.ToUniversalTime() - DateTime(1970,1,1,0,0,0,0,DateTimeKind.Utc)).TotalSeconds
let private makeLogins logins =
    match logins with
    | None -> ""
    | Some(l)  when Seq.isEmpty(l) -> ""
    | Some(l)  -> "userLogins=" + (l |> Seq.fold (fun a v -> a + v + ",") "").TrimEnd(',')
                                                                                   
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
    |SitesManager of SitesManagerMethod
    |Annotations of AnnotationsMethod
    |ExampleAPI of ExampleAPIMethod
    |ImageGraph of ImageGraphMethod
    |LanguagesManager of LanguagesManagerMethod
    |Live of LiveMethod
    |MobileMessaging of MobileMessagingMethod
    |Overlay of OverlayMethod
    |PDFReports of PDFReportsMethod
    |Transitions of TransitionsMethod
    |SEO of SEOMethod
    |UsersManager of UsersManagerMethod
    interface ApiParameter with
        member this.Command =
            match this with
            | MultiSites(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | VisitsSummary(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | VisitTime(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | Actions(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | Referers(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | Goals(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | UserCountry(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | CustomVariables(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | VisitorInterest(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | VisitFrequency(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | Provider(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | UserSettings(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name 
            | API(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | SitesManager(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | Annotations(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | ExampleAPI(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | ImageGraph(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | LanguagesManager(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | Live(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | MobileMessaging(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | Overlay(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | PDFReports(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | Transitions(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | SEO(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
            | UsersManager(n) -> methodTypeCmdImpl this n (fst( Reflection.FSharpValue.GetUnionFields(this,this.GetType()))).Name
        member this.Name = "method"
and UsersManagerMethod =
    | SetUserPreference of string * string * string
    | GetUserPreference of string * string
    | GetUsers of seq<string> option 
    | GetUsersLogin 
    | GetUsersSitesFromAccess of string
    | GetUsersAccessFromSite of SiteId 
    | GetUsersWithSiteAccess of SiteId * string
    | GetSitesAccessFromUser of string 
    | GetUser of string 
    | GetUserByEmail of string 
    | AddUser of string * string * string * string option 
    | UpdateUser of string * string option * string option * string option 
    | DeleteUser of string
    | UserExists of string
    | UserEmailExists of string
    | SetUserAccess of string * string * SiteId
    | GetTokenAuth of string * string
    interface ApiMethod with
        member this.Command =
                match this with
                | SetUserPreference(userLogin, preferenceName, preferenceValue) as t -> (methodCmdImpl t) + makeParams2([|("userLogin",userLogin);("preferenceName",preferenceName);("preferenceValue",preferenceValue)|]) 
                | GetUserPreference(userLogin, preferenceName) as t -> (methodCmdImpl t) + makeParams2([|("userLogin",userLogin);("preferenceName",preferenceName)|])
                | GetUsers(userLogins) as t -> (methodCmdImpl t) + makeLogins(userLogins)
                | GetUsersLogin as t -> methodCmdImpl t
                | GetUsersSitesFromAccess(access) as t -> (methodCmdImpl t) + makeParam("access",access)
                | GetUsersAccessFromSite(siteId) as t -> (methodCmdImpl t) + (siteId:>ApiParameter).Command 
                | GetUsersWithSiteAccess (siteId,access) as t -> (methodCmdImpl t) + (siteId:>ApiParameter).Command
                                                                 + makeParam("access",access)  
                | GetSitesAccessFromUser (userLogin) as t -> (methodCmdImpl t) + makeParam("userLogin",userLogin)
                | GetUser (userLogin) as t -> (methodCmdImpl t) + makeParam("userLogin",userLogin)
                | GetUserByEmail (userEmail) as t -> (methodCmdImpl t) + makeParam("userEmail",userEmail)
                | AddUser (userLogin, password, email, alias) as t -> (methodCmdImpl t) + makeParams2([|("userLogin",userLogin);("password",password);("email",email)|])
                                                                             + (if (alias.IsSome) then makeParam("alias",alias.Value) else "")
                | UpdateUser (userLogin, password , email , alias) as t -> (methodCmdImpl t) + makeParam("userLogin",userLogin) 
                                                                              + (if (password.IsSome) then makeParam("password",password.Value) else "") 
                                                                               + (if (email.IsSome) then makeParam("email",email.Value) else "") 
                                                                                + (if (alias.IsSome) then makeParam("alias",alias.Value) else "")   
                | DeleteUser (userLogin) as t -> (methodCmdImpl t) + makeParam("userLogin",userLogin) 
                | UserExists (userLogin) as t -> (methodCmdImpl t) + makeParam("userLogin",userLogin) 
                | UserEmailExists (userEmail) as t -> (methodCmdImpl t) + makeParam("userEmail",userEmail) 
                | SetUserAccess (userLogin, access, idSites) as t -> (methodCmdImpl t) + makeParams2([|("userLogin",userLogin);("access",access)|]) + (idSites:>ApiParameter).Command 
                | GetTokenAuth (userLogin, md5Password) as t -> (methodCmdImpl t) + makeParams2([|("userLogin",userLogin);("md5Password",md5Password)|])
and SEOMethod =
    | GetRank of string
    interface ApiMethod with
        member this.Command =
                match this with
                | GetRank(url) as t -> (methodCmdImpl t) + makeParam("url",url)
and TransitionsMethod =
    | GetTransitionsForPageTitle of SiteId * TimeSlice * string * SegmentType option * string option 
    | GetTransitionsForPageUrl of SiteId * TimeSlice * string * SegmentType option * string option
    | GetTransitionsForAction  of SiteId * TimeSlice * string * string * string option *  SegmentType option * string option * string option 
    | GetTranslations
    interface ApiMethod with
        member this.Command =
                match this with
                | GetTransitionsForPageTitle(sid,ts,pt,segment ,limitBeforeGrouping) as t -> (methodCmdImpl t)
                                                                                             + ( makeParams [|sid:>ApiParameter; ts:>ApiParameter|])
                                                                                             + makeParam("pageTitle",pt)
                                                                                             + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                                             + (if (limitBeforeGrouping.IsSome) then makeParam("limitBeforeGrouping",limitBeforeGrouping.Value) else "") 
                | GetTransitionsForPageUrl(sid,ts,pu,segment ,limitBeforeGrouping) as t -> (methodCmdImpl t)
                                                                                             + ( makeParams [|sid:>ApiParameter; ts:>ApiParameter|])
                                                                                             + makeParam("pageUrl",pu)
                                                                                             + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                                             + (if (limitBeforeGrouping.IsSome) then makeParam("limitBeforeGrouping",limitBeforeGrouping.Value) else "") 
                | GetTransitionsForAction(sid,ts,an,at,parts,
                                            segment ,limitBeforeGrouping
                                            ,returnNormalizedUrls) as t -> (methodCmdImpl t)
                                                                           + ( makeParams [|sid:>ApiParameter; ts:>ApiParameter|])
                                                                           + makeParam("actionName",an)+ makeParam("actionType",at)
                                                                           + (if (parts.IsSome) then makeParam("parts",parts.Value) else makeParam("parts","All") ) 
                                                                           + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                           + (if (limitBeforeGrouping.IsSome) then makeParam("limitBeforeGrouping",limitBeforeGrouping.Value) else "")
                                                                           + (if (returnNormalizedUrls.IsSome) then makeParam("returnNormalizedUrls",returnNormalizedUrls.Value) else "")  
                | GetTranslations as t  -> methodCmdImpl t
and PDFReportsMethod =
    | AddReport of SiteId * PeriodType * string *  string * string * string * string
    | UpdateReport of SiteId * PeriodType * string * string* string * string * string * string 
    | DeleteReport of string
    | GetReports of SiteId option * PeriodType option * string option * string option 
    | GenerateReport of string * DateSlice * string option * string option * PeriodType option * string option * string option
    | SendReport of string * TimeSlice option 
    interface ApiMethod with
        member this.Command =
                match this with
                | AddReport (siteId, periodType,description,  reportType, reportFormat, reports, parameters) as t -> (methodCmdImpl t)
                                                                                                                         + (makeParam2 siteId) 
                                                                                                                         + periodType.Command
                                                                                                                         + (makeParams2 [|("description",description);
                                                                                                                                            ("reportType",reportType);
                                                                                                                                            ("reportFormat",reportFormat);
                                                                                                                                            ("reports",reports);
                                                                                                                                            ("parameters",parameters)|])
                |UpdateReport (siteId, periodType,idReport, description, reportType, reportFormat, reports, parameters) as t -> (methodCmdImpl t)
                                                                                                                                 + (makeParam2 siteId) 
                                                                                                                                 + periodType.Command
                                                                                                                                 + (makeParams2 [|("idReport",idReport);
                                                                                                                                            ("description",description);
                                                                                                                                            ("reportType",reportType);
                                                                                                                                            ("reportFormat",reportFormat);
                                                                                                                                            ("reports",reports);
                                                                                                                                            ("parameters",parameters)|])
                | DeleteReport (idReport)as t -> (methodCmdImpl t) + makeParam("idReport",idReport)
                | GetReports (idSite , period, idReport , ifSuperUserReturnOnlySuperUserReports ) as t -> (methodCmdImpl t)
                                                                                                            + (if (idSite.IsSome) then (makeParam2 idSite.Value) else "")
                                                                                                            + (if (period.IsSome) then (period.Value.Command) else "")
                                                                                                            + (if (idReport.IsSome) then makeParam("idReport",idReport.Value) else "" ) 
                                                                                                            + (if (ifSuperUserReturnOnlySuperUserReports.IsSome) then makeParam("ifSuperUserReturnOnlySuperUserReports",ifSuperUserReturnOnlySuperUserReports.Value) else "" ) 
                | GenerateReport (idReport, date, language, outputType, period , reportFormat, parameters) as t -> (methodCmdImpl t)
                                                                                                                 +  makeParam("idReport",idReport) 
                                                                                                                 +  (date:>ApiParameter).Command
                                                                                                                 + (if (language.IsSome) then makeParam("language",language.Value) else "" ) 
                                                                                                                 + (if (outputType.IsSome) then makeParam("outputType",outputType.Value) else "" ) 
                                                                                                                 + (if (period.IsSome) then ( period.Value.Command) else "")
                                                                                                                 + (if (reportFormat.IsSome) then makeParam("reportFormat",reportFormat.Value) else "" ) 
                                                                                                                 + (if (parameters.IsSome) then makeParam("parameters",parameters.Value) else "" ) 
                | SendReport (idReport, timeSlice) as t -> (methodCmdImpl t) + (if (timeSlice.IsSome) then (makeParam2 timeSlice.Value) else "")


and OverlayMethod =
    | GetTranslations of SiteId 
    | GetExcludedQueryParameters of SiteId 
    | GetFollowingPages  of SiteId * TimeSlice * string * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetTranslations(sid) as t -> (methodCmdImpl t) +  (makeParam2 sid)
                | GetExcludedQueryParameters(sid) as t -> (methodCmdImpl t) +  (makeParam2 sid)
                | GetFollowingPages(sid,ts,url,segment) as t -> (methodCmdImpl t) + ( makeParam2 sid) + (makeParam2 ts) + makeParam("url",url)
                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 

and MobileMessagingMethod =
    | AreSMSAPICredentialProvided 
    | GetSMSProvider 
    | SetSMSAPICredential of string * string
    | AddPhoneNumber of string
    | SanitizePhoneNumber of string
    | GetCreditLeft 
    | RemovePhoneNumber of string
    | ValidatePhoneNumber of string * string
    | DeleteSMSAPICredential 
    | SetDelegatedManagement of string
    | GetDelegatedManagement 
    interface ApiMethod with
        member this.Command =
                match this with
                | AreSMSAPICredentialProvided|GetSMSProvider|GetCreditLeft|DeleteSMSAPICredential|GetDelegatedManagement as t -> methodCmdImpl t 
                | SetSMSAPICredential(provider, apiKey) as t -> methodCmdImpl t  + (makeParams2 [|("provider",provider);("apiKey",apiKey)|])
                | AddPhoneNumber (phoneNumber) | SanitizePhoneNumber (phoneNumber) | RemovePhoneNumber (phoneNumber)  as t -> methodCmdImpl t + makeParam("phoneNumber",phoneNumber)
                | ValidatePhoneNumber (phoneNumber, verificationCode) as t -> methodCmdImpl t + (makeParams2 [|("phoneNumber",phoneNumber);("verificationCode",verificationCode)|])
                | SetDelegatedManagement (delegatedManagement) as t -> methodCmdImpl t + makeParam("delegatedManagement",delegatedManagement)
and LiveMethod =
    | GetCounters  of SiteId * int * SegmentType option
    | GetLastVisitsDetails of SiteId * TimeSlice * SegmentType option  * int option * int option * DateTime option 
    interface ApiMethod with
        member this.Command =
                match this with
                | GetCounters (idSite, lastMinutes, segment) as t -> methodCmdImpl t + makeParam("lastMinutes",lastMinutes)
                                                                         + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                     
                | GetLastVisitsDetails (sid, ts, segment, filter_limit , maxIdVisit , minTimestamp ) as t -> methodCmdImpl t 
                                                                                                            + ( makeParams [|sid:>ApiParameter; ts:>ApiParameter|])
                                                                                                            + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                                                            + (if (filter_limit.IsSome) then makeParam("filter_limit",filter_limit.Value) else "" )
                                                                                                            + (if (maxIdVisit.IsSome) then makeParam("maxIdVisit",maxIdVisit.Value) else "" )
                                                                                                            + (if (minTimestamp.IsSome) then makeParam("minTimestamp",getUNIXTimeStamp minTimestamp.Value) else "" )

and LanguagesManagerMethod =
    | IsLanguageAvailable of string
    | GetAvailableLanguages 
    | GetAvailableLanguagesInfo 
    | GetAvailableLanguageNames 
    | GetTranslationsForLanguage of string
    | GetLanguageForUser of string
    | SetLanguageForUser of string * string
    interface ApiMethod with
        member this.Command =
                match this with
                |  IsLanguageAvailable (languageCode) | GetTranslationsForLanguage(languageCode) as t -> methodCmdImpl t  + makeParam("languageCode",languageCode)
                | GetAvailableLanguages | GetAvailableLanguagesInfo | GetAvailableLanguageNames  as t -> methodCmdImpl t
                | GetLanguageForUser (login) as t -> methodCmdImpl t  + makeParam("login",login)
                | SetLanguageForUser (login,languageCode) as t -> methodCmdImpl t  + makeParams2[|("login",login);("languageCode",languageCode)|]
and ImageGraphMethod =
    | Get
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and ExampleAPIMethod =
    | GetPiwikVersion //
    | GetAnswerToLife //() 
    | GetObject //() 
    | GetSum //(a = '0', b = '0') 
    | GetNull //() 
    | GetDescriptionArray //() 
    | GetCompetitionDatatable //() 
    | GetMoreInformationAnswerToLife //() 
    | GetMultiArray //() 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and CustomVariablesMethod =
    | GetCustomVariables of SiteId * TimeSlice * SegmentType option * bool option
    | GetCustomVariablesValuesFromNameId of SiteId * TimeSlice * string * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetCustomVariables (sid,ts,segment, expanded)  as t -> methodCmdImpl t  + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                          + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                                          + (if (expanded.IsSome && expanded.Value) then makeParam("expanded",1) else "" )
                | GetCustomVariablesValuesFromNameId (sid,ts, idSubtable, segment )   as t -> methodCmdImpl t  + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                                                + makeParam("idSubtable",idSubtable)
                                                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                                                                                                                         
and MultiSitesMethod =
    | GetOne of SiteId * TimeSlice * SegmentType option * bool option 
    | GetAll of TimeSlice * SegmentType option * bool option * string option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetOne(sid,ts,segment,enhanced) as t -> (methodCmdImpl t) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                     + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                     + (if (enhanced.IsSome) then makeParam("enhanced",enhanced.Value) else "" )
                                                                     
                | GetAll(ts,segment,enhanced,pattern) as t -> (methodCmdImpl t) + (ts:>ApiParameter).Command
                                                                  + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "") 
                                                                     + (if (enhanced.IsSome) then makeParam("enhanced",enhanced.Value) else "" )
                                                                     + (if (pattern.IsSome) then makeParam("pattern",pattern.Value) else "" )

and VisitsSummaryMethod =
    | Get
    | GetVisits of SiteId * TimeSlice * SegmentType option
    | GetUniqueVisitors of SiteId * TimeSlice * SegmentType option
    | GetActions of SiteId * TimeSlice * SegmentType option
    | GetMaxActions of SiteId * TimeSlice * SegmentType option
    | GetBounceCount of SiteId * TimeSlice * SegmentType option
    | GetVisitsConverted of SiteId * TimeSlice * SegmentType option
    | GetSumVisitsLength of SiteId * TimeSlice * SegmentType option
    | GetSumVisitsLengthPretty of SiteId * TimeSlice * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | Get as t -> methodCmdImpl t 
                |GetVisits (sid,ts,segment) | GetUniqueVisitors (sid,ts,segment) | GetActions  (sid,ts,segment)
                | GetMaxActions (sid,ts,segment)| GetBounceCount (sid,ts,segment)
                | GetVisitsConverted  (sid,ts,segment)| GetSumVisitsLength (sid,ts,segment)
                | GetSumVisitsLengthPretty (sid,ts,segment) as t -> (methodCmdImpl t) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                     + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")  
and VisitTimeMethod =
    | GetVisitInformationPerServerTime of SiteId * TimeSlice * SegmentType option * bool option
    | GetVisitInformationPerLocalTime of SiteId * TimeSlice * SegmentType option
    | GetByDayOfWeek of SiteId * TimeSlice * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetVisitInformationPerLocalTime(sid,ts,segment)
                | GetByDayOfWeek(sid,ts,segment) as t -> (methodCmdImpl t) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                     + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")  
                | GetVisitInformationPerServerTime(sid,ts,segment,hfhwt)as t -> (methodCmdImpl t) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                                    + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")  
                                                                                                    + (if (hfhwt.IsSome) then makeParam("hideFutureHoursWhenToday",hfhwt.Value) else "" )

and ActionsMethod =
    | Get of SiteId * TimeSlice * SegmentType option * string option//(idSite, period, date, segment = '', columns = '')  	 
    | GetPageUrls of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageUrlsFollowingSiteSearch of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageTitlesFollowingSiteSearch of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetEntryPageUrls of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetExitPageUrls of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageUrl of string * SiteId * TimeSlice * SegmentType option//(pageUrl, idSite, period, date, segment = '') 
    | GetPageTitles of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetEntryPageTitles of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetExitPageTitles of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageTitle of string * SiteId * TimeSlice * SegmentType option//(pageName, idSite, period, date, segment = '') 
    | GetDownloads of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetDownload of string * SiteId * TimeSlice * SegmentType option//(downloadUrl, idSite, period, date, segment = '') 
    | GetOutlinks of SiteId * TimeSlice * SegmentType option *  bool option * string option//(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetOutlink  of string * SiteId * TimeSlice * SegmentType option//(outlinkUrl, idSite, period, date, segment = '') 
    | GetSiteSearchKeywords of SiteId * TimeSlice * SegmentType option//(idSite, period, date, segment = '')  	 
    | AddPagesPerSearchColumn of string * string option //(dataTable, columnToRead = 'nb_hits') 
    | GetSiteSearchNoResultKeywords of SiteId * TimeSlice * SegmentType option //(idSite, period, date, segment = '')  	 
    | GetSiteSearchCategories  of SiteId * TimeSlice * SegmentType option//(idSite, period, date, segment = '') 
    interface ApiMethod with
        member this.Command =
                match this with
                | Get(sid,ts,segment,columns) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                        + (if (columns.IsSome) then makeParam("columns",columns.Value) else "" ) 
                | GetPageUrls(sid,ts,segment,expanded,idSubtable)
                | GetPageUrlsFollowingSiteSearch(sid,ts,segment,expanded,idSubtable)
                | GetPageTitlesFollowingSiteSearch(sid,ts,segment,expanded,idSubtable)
                | GetEntryPageUrls(sid,ts,segment,expanded,idSubtable)
                | GetExitPageUrls(sid,ts,segment,expanded,idSubtable)
                | GetPageTitles(sid,ts,segment,expanded,idSubtable)
                | GetEntryPageTitles(sid,ts,segment,expanded,idSubtable)
                | GetExitPageTitles(sid,ts,segment,expanded,idSubtable)
                | GetDownloads(sid,ts,segment,expanded,idSubtable)
                | GetOutlinks(sid,ts,segment,expanded,idSubtable) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                        + (if (expanded.IsSome && expanded.Value) then makeParam("expanded",1) else "" ) 
                                                                        + (if (idSubtable.IsSome) then makeParam("idSubtable",idSubtable.Value) else "" )
                | GetPageUrl(pageUrl,sid,ts,segment) as t -> methodCmdImpl t + makeParam("pageUrl",pageUrl) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetPageTitle(pageName,sid,ts,segment) as t -> methodCmdImpl t + makeParam("pageName",pageName) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetDownload(downloadUrl,sid,ts,segment) as t -> methodCmdImpl t + makeParam("downloadUrl",downloadUrl) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetOutlink(outlinkUrl,sid,ts,segment) as t -> methodCmdImpl t + makeParam("outlinkUrl",outlinkUrl) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetSiteSearchKeywords(sid,ts,segment)
                | GetSiteSearchNoResultKeywords(sid,ts,segment)
                | GetSiteSearchCategories(sid,ts,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")                                                        
                | AddPagesPerSearchColumn(dataTable,columnToRead)as t -> methodCmdImpl t + makeParam("dataTable",dataTable)
                                                                        + (if (columnToRead.IsSome ) then makeParam("columnToRead",columnToRead.Value) else makeParam("columnToRead","nb_hits")  )   
and ReferersMethod =
    | GetRefererType of SiteId * TimeSlice * SegmentType option * string option  * string option * bool option
    | GetAll of SiteId * TimeSlice * SegmentType option
    | GetKeywords of SiteId * TimeSlice * SegmentType option * bool option
    | GetKeywordsForPageUrl of SiteId * TimeSlice * string
    | GetKeywordsForPageTitle of SiteId * TimeSlice * string
    | GetSearchEnginesFromKeywordId of SiteId * TimeSlice * string * SegmentType option 
    | GetSearchEngines of SiteId * TimeSlice * SegmentType option * bool option
    | GetKeywordsFromSearchEngineId of SiteId * TimeSlice * string * SegmentType option 
    | GetCampaigns of SiteId * TimeSlice * SegmentType option * bool option
    | GetKeywordsFromCampaignId  of SiteId * TimeSlice * string * SegmentType option
    | GetWebsites of SiteId * TimeSlice * SegmentType option * bool option
    | GetUrlsFromWebsiteId of SiteId * TimeSlice * string * SegmentType option
    | GetSocials of SiteId * TimeSlice * SegmentType option * bool option
    | GetUrlsForSocial of SiteId * TimeSlice  * SegmentType option * string option
    | GetNumberOfDistinctSearchEngines of SiteId * TimeSlice  * SegmentType option
    | GetNumberOfDistinctKeywords of SiteId * TimeSlice  * SegmentType option
    | GetNumberOfDistinctCampaigns of SiteId * TimeSlice  * SegmentType option
    | GetNumberOfDistinctWebsites of SiteId * TimeSlice  * SegmentType option
    | GetNumberOfDistinctWebsitesUrls of SiteId * TimeSlice  * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetRefererType(sid,ts,segment,typeReferer,idSubtable,expanded) as t ->  methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                                            + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                                            + (if (typeReferer.IsSome) then makeParam("typeReferer",typeReferer.Value) else "" )
                                                                                                            + (if (expanded.IsSome && expanded.Value) then makeParam("expanded",1) else "" ) 
                                                                                                            + (if (idSubtable.IsSome) then makeParam("idSubtable",idSubtable.Value) else "" )
                | GetAll(sid,ts,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetKeywords(sid,ts,segment,expanded) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                        + (if (expanded.IsSome && expanded.Value) then makeParam("expanded",1) else "" ) 
                | GetKeywordsForPageUrl(sid,ts,url) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command + makeParam("url",url) 
                | GetKeywordsForPageTitle(sid,ts,title) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command + makeParam("title",title) 
                | GetSearchEnginesFromKeywordId(sid,ts,idSubtable,segment)
                | GetKeywordsFromCampaignId(sid,ts,idSubtable,segment)
                | GetUrlsFromWebsiteId(sid,ts,idSubtable,segment)
                | GetKeywordsFromSearchEngineId(sid,ts,idSubtable,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command + makeParam("idSubtable",idSubtable)
                                                                                                    + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")  
                | GetSearchEngines(sid,ts,segment,expanded)
                | GetWebsites(sid,ts,segment,expanded)
                | GetSocials(sid,ts,segment,expanded)
                | GetCampaigns(sid,ts,segment,expanded) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                        + (if (expanded.IsSome && expanded.Value) then makeParam("expanded",1) else "" ) 
                |GetUrlsForSocial(sid,ts,segment,idSubtable) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                        + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                        + (if (idSubtable.IsSome) then makeParam("idSubtable",idSubtable.Value) else "" ) 
                | GetNumberOfDistinctSearchEngines (sid,ts,segment)
                | GetNumberOfDistinctKeywords (sid,ts,segment)
                | GetNumberOfDistinctCampaigns (sid,ts,segment)
                | GetNumberOfDistinctWebsites (sid,ts,segment)
                | GetNumberOfDistinctWebsitesUrls (sid,ts,segment)  as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                                                                                                                  
                                                                         
and GoalsMethod =
    | GetGoals of SiteId
    | AddGoal  of SiteId * string * string * string * string * bool option * string option * bool option//(idSite, name, matchAttribute, pattern, patternType, caseSensitive = '', revenue = '', allowMultipleConversionsPerVisit = '') [ No example available ]
    | UpdateGoal of SiteId * string * string * string * string * string * bool option * string option * bool option//(idSite, idGoal, name, matchAttribute, pattern, patternType, caseSensitive = '', revenue = '', allowMultipleConversionsPerVisit = '') [ No example available ]
    | DeleteGoal of SiteId * string//(idSite, idGoal) [ No example available ]
    | GetItemsSku of SiteId * TimeSlice * bool option//(idSite, period, date, abandonedCarts = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetItemsName of SiteId * TimeSlice * bool option//(idSite, period, date, abandonedCarts = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetItemsCategory of SiteId * TimeSlice * bool option//(idSite, period, date, abandonedCarts = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | Get of SiteId * TimeSlice * SegmentType option * string option * string option//(idSite, period, date, segment = '', idGoal = '', columns = 'Array') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetDaysToConversion of SiteId * TimeSlice * SegmentType option * string option//(idSite, period, date, segment = '', idGoal = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetVisitsUntilConversion of SiteId * TimeSlice * SegmentType option * string option//(idSite, period, date, segment = '', idGoal = '') [ Example in XML, Json, Tsv 
    interface ApiMethod with
        member this.Command =
                match this with
                | GetGoals (sid)  as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                | AddGoal (sid, name, matchAttribute, pattern, patternType, caseSensitive , revenue , allowMultipleConversionsPerVisit )as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                                                                                                                                                                    + (makeParams2 [|("name",name);("matchAttribute",matchAttribute);("pattern",pattern);("patternType",patternType)|])
                                                                                                                                                                    + (if (caseSensitive.IsSome) then makeParam("caseSensitive",caseSensitive.Value) else "" )
                                                                                                                                                                    + (if (revenue.IsSome) then makeParam("revenue",revenue.Value) else "" )
                                                                                                                                                                    + (if (allowMultipleConversionsPerVisit.IsSome) then makeParam("allowMultipleConversionsPerVisit",allowMultipleConversionsPerVisit.Value) else "" )
                | UpdateGoal (sid, idGoal, name, matchAttribute, pattern, patternType, caseSensitive , revenue , allowMultipleConversionsPerVisit )as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                                                                                                                                                                    + (makeParams2 [|("idGoal",idGoal);("name",name);("matchAttribute",matchAttribute);("pattern",pattern);("patternType",patternType)|])
                                                                                                                                                                    + (if (caseSensitive.IsSome) then makeParam("caseSensitive",caseSensitive.Value) else "" )
                                                                                                                                                                    + (if (revenue.IsSome) then makeParam("revenue",revenue.Value) else "" )
                                                                                                                                                                    + (if (allowMultipleConversionsPerVisit.IsSome) then makeParam("allowMultipleConversionsPerVisit",allowMultipleConversionsPerVisit.Value) else "" )
                | DeleteGoal (sid, idGoal)as t -> methodCmdImpl t + (sid:>ApiParameter).Command + makeParam("idGoal",idGoal)
                | GetItemsSku (sid,ts,abandonedCarts) 
                | GetItemsName (sid,ts,abandonedCarts) 
                | GetItemsCategory (sid,ts,abandonedCarts) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (abandonedCarts.IsSome) then makeParam("abandonedCarts",abandonedCarts) else "" ) 
                | Get (sid,ts,segment,idGoal,columns) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                + (if (idGoal.IsSome) then makeParam("idGoal",idGoal.Value) else "" ) 
                                                                                + (if (columns.IsSome) then makeParam("columns",columns.Value) else "" ) 
                | GetDaysToConversion (sid,ts,segment,idGoal)
                | GetVisitsUntilConversion (sid,ts,segment,idGoal) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                + (if (idGoal.IsSome) then makeParam("idGoal",idGoal.Value) else "" ) 
                                                                                
                                                                                
and UserCountryMethod =
    | GetCountry of SiteId * TimeSlice * SegmentType option
    | GetContinent of SiteId * TimeSlice * SegmentType option
    | GetRegion of SiteId * TimeSlice * SegmentType option
    | GetCity of SiteId * TimeSlice * SegmentType option
    | GetLocationFromIP of string * string option
    | GetNumberOfDistinctCountries of SiteId * TimeSlice * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetCountry (sid,ts,segment)
                | GetContinent (sid,ts,segment)
                | GetRegion (sid,ts,segment)
                | GetNumberOfDistinctCountries (sid,ts,segment)
                | GetCity (sid,ts,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetLocationFromIP (ip, provider) as t -> methodCmdImpl t + makeParam("ip",ip)
                                                                                + (if (provider.IsSome) then makeParam("provider",provider.Value) else "" ) 
                                                                                

and VisitorInterestMethod =
    | GetNumberOfVisitsPerVisitDuration of SiteId * TimeSlice * SegmentType option
    | GetNumberOfVisitsPerPage of SiteId * TimeSlice * SegmentType option
    | GetNumberOfVisitsByVisitCount of SiteId * TimeSlice * SegmentType option
    | GetNumberOfVisitsByDaysSinceLast of SiteId * TimeSlice * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetNumberOfVisitsPerVisitDuration (sid,ts,segment)
                | GetNumberOfVisitsPerPage (sid,ts,segment)
                | GetNumberOfVisitsByVisitCount (sid,ts,segment)
                | GetNumberOfVisitsByDaysSinceLast (sid,ts,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
and VisitFrequencyMethod =
    | Get of SiteId * TimeSlice * SegmentType option * string option
    interface ApiMethod with
        member this.Command =
                match this with
                | Get (sid,ts,segment,columns) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                + (if (columns.IsSome) then makeParam("columns",columns.Value) else "" ) 
and ProviderMethod =
    | GetProvider  of SiteId * TimeSlice * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetProvider(sid,ts,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
and UserSettingsMethod =
    | GetResolution  of SiteId * TimeSlice * SegmentType option
    | GetBrowser  of SiteId * TimeSlice * SegmentType option
    | GetBrowserVersion  of SiteId * TimeSlice * SegmentType option
    | GetBrowserType  of SiteId * TimeSlice * SegmentType option
    | GetPlugin  of SiteId * TimeSlice * SegmentType option
    | GetWideScreen  of SiteId * TimeSlice * SegmentType option
    | GetOS  of SiteId * TimeSlice * SegmentType option *  string option
    | GetConfiguration  of SiteId * TimeSlice * SegmentType option
    | GetOSFamily  of SiteId * TimeSlice * SegmentType option
    | GetMobileVsDesktop  of SiteId * TimeSlice * SegmentType option
    interface ApiMethod with
        member this.Command =
                match this with
                | GetResolution(sid,ts,segment)
                | GetBrowser(sid,ts,segment)
                | GetBrowserVersion(sid,ts,segment)
                | GetBrowserType(sid,ts,segment)
                | GetPlugin(sid,ts,segment)
                | GetWideScreen(sid,ts,segment)
                | GetConfiguration(sid,ts,segment)
                | GetOSFamily(sid,ts,segment)
                | GetMobileVsDesktop(sid,ts,segment) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                | GetOS(sid,ts,segment,addShortLabel ) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                + (if (addShortLabel.IsSome) then makeParam("addShortLabel",addShortLabel.Value) else makeParam("addShortLabel",1) )

and APIMethod =
    | GetPiwikVersion //() 
    | GetSettings //() 
    | GetDefaultMetricTranslations //() 
    | GetDefaultMetrics //() 
    | GetDefaultProcessedMetrics //() 
    | GetDefaultMetricsDocumentation //() 
    | GetSegmentsMetadata of SiteId option//(idSites = 'Array') 
    | GetLogoUrl of string option //(pathOnly = '') 
    | GetHeaderLogoUrl of string option//(pathOnly = '') 
    | GetMetadata  of SiteId * string * string * string option * string option * TimeSlice option * bool option * bool option//(idSite, apiModule, apiAction, apiParameters = 'Array', language = '', period = '', date = '', hideMetricsDoc = '', showSubtableReports = '')  	 
    | GetReportMetadata of SiteId option * TimeSlice option * bool option * bool option //(idSites = '', period = '', date = '', hideMetricsDoc = '', showSubtableReports = '')  	 
    | GetProcessedReport of SiteId * TimeSlice * string * string * SegmentType option * string option * string option * string option * bool option * bool option * string option * bool option //(idSite, period, date, apiModule, apiAction, segment = '', apiParameters = '', idGoal = '', language = '', showTimer = '1', hideMetricsDoc = '', idSubtable = '', showRawMetrics = '')  	 
    | Get of SiteId * TimeSlice * SegmentType option * string option//(idSite, period, date, segment = '', columns = '')  	 
    | GetRowEvolution of SiteId * TimeSlice * string * string * string option * SegmentType option * string option * string option * string option * string option * string option//(idSite, period, date, apiModule, apiAction, label = '', segment = '', column = '', language = '', idGoal = '', legendAppendMetric = '1', labelUseAbsoluteUrl = '1')  	 
    | GetBulkRequest of string//(urls) 
    interface ApiMethod with
        member this.Command =
                match this with
                | GetSegmentsMetadata(sid) as t -> methodCmdImpl t +  (if (sid.IsSome) then (sid.Value:>ApiParameter).Command else "")
                | GetLogoUrl(pathOnly)
                | GetHeaderLogoUrl(pathOnly)  as t -> methodCmdImpl t + (if (pathOnly.IsSome) then makeParam("pathOnly",pathOnly) else "" )
                | GetMetadata(sid, apiModule, apiAction, apiParameters, language , ts, hideMetricsDoc, showSubtableReports)  as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                                                                                                                                             + makeParams2 [|("apiModule",apiModule);("apiAction",apiAction)|]
                                                                                                                                             + (if (apiParameters.IsSome) then makeParam("apiParameters",apiParameters.Value) else "" )
                                                                                                                                             + (if (language.IsSome) then makeParam("language",language.Value) else "" )
                                                                                                                                             + (if (ts.IsSome) then (ts.Value:>ApiParameter).Command else "" )
                                                                                                                                             + (if (hideMetricsDoc.IsSome) then makeParam("hideMetricsDoc",hideMetricsDoc.Value) else "" )
                                                                                                                                             + (if (showSubtableReports.IsSome) then makeParam("showSubtableReports",showSubtableReports.Value) else "" )
                | GetReportMetadata(sid,ts,hideMetricsDoc, showSubtableReports)  as t -> methodCmdImpl t + (if (sid.IsSome) then (sid.Value:>ApiParameter).Command else "" )
                                                                                                            + (if (ts.IsSome) then (ts.Value:>ApiParameter).Command else "" )
                                                                                                                                             + (if (hideMetricsDoc.IsSome) then makeParam("hideMetricsDoc",hideMetricsDoc.Value) else "" )
                                                                                                                                             + (if (showSubtableReports.IsSome) then makeParam("showSubtableReports",showSubtableReports.Value) else "" )
                | GetProcessedReport(sid, ts, apiModule, apiAction, segment ,
                                        apiParameters , idGoal , language ,
                                        showTimer , hideMetricsDoc , idSubtable , showRawMetrics) as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                                                                                                                          + (ts:>ApiParameter).Command 
                                                                                                                           + makeParams2 [|("apiModule",apiModule);("apiAction",apiAction)|]
                                                                                                                           + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                                                            + (if (apiParameters.IsSome) then makeParam("apiParameters",apiParameters.Value) else "" )
                                                                                                                             + (if (idGoal.IsSome) then makeParam("idGoal",idGoal.Value) else "" )
                                                                                                                             + (if (language.IsSome) then makeParam("language",language.Value) else "" )
                                                                                                                             + (if (showTimer.IsSome) then makeParam("showTimer",showTimer.Value) else makeParam("showTimer",1)  )
                                                                                                                              + (if (hideMetricsDoc.IsSome) then makeParam("hideMetricsDoc",hideMetricsDoc.Value) else "" )
                                                                                                                               + (if (idSubtable.IsSome) then makeParam("idSubtable",idSubtable.Value) else "" )
                                                                                                                               + (if (showRawMetrics.IsSome) then makeParam("showRawMetrics",showRawMetrics.Value) else "" )
                                                                                                                            
                | Get(sid, ts, segment , columns)as t -> methodCmdImpl t + (sid:>ApiParameter).Command  + (ts:>ApiParameter).Command 
                                                                          + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                           + (if (columns.IsSome) then makeParam("columns",columns.Value) else "" ) 
                | GetRowEvolution(sid, ts, apiModule, apiAction,
                                     label , segment , column , language ,
                                      idGoal , legendAppendMetric, labelUseAbsoluteUrl) as t -> methodCmdImpl t  + (sid:>ApiParameter).Command  + (ts:>ApiParameter).Command 
                                                                                                                  + makeParams2 [|("apiModule",apiModule);("apiAction",apiAction)|] 
                                                                                                                   + (if (label.IsSome) then makeParam("label",label.Value) else "" )
                                                                                                                    + (if (segment.IsSome) then (segment.Value:>ApiParameter).Command else "")
                                                                                                                     + (if (column.IsSome) then makeParam("column",column.Value) else "" )
                                                                                                                      + (if (language.IsSome) then makeParam("language",language.Value) else "" )
                                                                                                                       + (if (idGoal.IsSome) then makeParam("idGoal",idGoal.Value) else "" )
                                                                                                                        + (if (legendAppendMetric.IsSome) then makeParam("legendAppendMetric",legendAppendMetric.Value) else makeParam("legendAppendMetric",1))
                                                                                                                         + (if (labelUseAbsoluteUrl.IsSome) then makeParam("labelUseAbsoluteUrl",labelUseAbsoluteUrl.Value) else makeParam("labelUseAbsoluteUrl",1) )
                                                                                                                                                                                                                            
                | GetBulkRequest(urls) as t -> methodCmdImpl t + makeParam("urls",urls)
                | _ as t -> methodCmdImpl t 
and SitesManagerMethod =
    | GetJavascriptTag of SiteId * string option //(idSite, piwikUrl = '') 
    | GetSitesFromGroup of string//(group) 
    | GetSitesGroups// () 
    | GetSiteFromId of SiteId//(idSite) 
    | GetSiteUrlsFromId of SiteId//(idSite) 
    | GetAllSites //() 
    | GetAllSitesId //() 
    | GetSitesIdWithVisits of DateTime option//(timestamp = '') 
    | GetSitesWithAdminAccess //() 
    | GetSitesWithViewAccess //() 
    | GetSitesWithAtLeastViewAccess of int option// (limit = '') 
    | GetSitesIdWithAdminAccess //() 
    | GetSitesIdWithViewAccess //() 
    | GetSitesIdWithAtLeastViewAccess// () 
    | GetSitesIdFromSiteUrl of string //(url) 
    | AddSite of string * string * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option //(siteName, urls, ecommerce = '', siteSearch = '', searchKeywordParameters = '', searchCategoryParameters = '', excludedIps = '', excludedQueryParameters = '', timezone = '', currency = '', group = '', startDate = '', excludedUserAgents = '') 
    | DeleteSite of SiteId// (idSite) 
    | AddSiteAliasUrls of SiteId * string //(idSite, urls) 
    | GetIpsForRange of string//(ipRange) 
    | SetGlobalExcludedIps of string //(excludedIps) 
    | SetGlobalSearchParameters of string *  string//(searchKeywordParameters, searchCategoryParameters) 
    | GetSearchKeywordParametersGlobal // () 
    | GetSearchCategoryParametersGlobal //() 
    | GetExcludedQueryParametersGlobal //() 
    | GetExcludedUserAgentsGlobal //() 
    | SetGlobalExcludedUserAgents of string //(excludedUserAgents) 
    | IsSiteSpecificUserAgentExcludeEnabled //() 
    | SetSiteSpecificUserAgentExcludeEnabled of string //(enabled) 
    | SetGlobalExcludedQueryParameters of string//(excludedQueryParameters) 
    | GetExcludedIpsGlobal //() 
    | GetDefaultCurrency //() 
    | SetDefaultCurrency of string //(defaultCurrency) 
    | GetDefaultTimezone //() 
    | SetDefaultTimezone //(defaultTimezone) 
    | UpdateSite of SiteId * string * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option * string option//(idSite, siteName, urls = '', ecommerce = '', siteSearch = '', searchKeywordParameters = '', searchCategoryParameters = '', excludedIps = '', excludedQueryParameters = '', timezone = '', currency = '', group = '', startDate = '', excludedUserAgents = '') 
    | GetCurrencyList //() 
    | GetCurrencySymbols// () 
    | GetTimezonesList //() 
    | GetUniqueSiteTimezones// () 
    | GetPatternMatchSites of string //(pattern) 
    interface ApiMethod with
        member this.Command =
                match this with
                | GetJavascriptTag(sid, piwikUrl) as t -> methodCmdImpl t + (sid:>ApiParameter).Command  +  (if (piwikUrl.IsSome) then makeParam("piwikUrl",piwikUrl.Value) else "" )
                | GetSitesFromGroup(group) as t -> methodCmdImpl t + makeParam("group",group)
                | GetSiteFromId(sid) | GetSiteUrlsFromId(sid)|DeleteSite(sid) as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                | GetSitesIdWithVisits (timestamp) as t -> methodCmdImpl t +  (if (timestamp.IsSome) then makeParam("timestamp",getUNIXTimeStamp timestamp.Value) else "" )
                | GetSitesWithAtLeastViewAccess (limit) as t -> methodCmdImpl t +  (if (limit.IsSome) then makeParam("limit",limit.Value) else "" )
                | GetSitesIdFromSiteUrl (url) as t -> methodCmdImpl t + makeParam("url",url)
                | AddSite (siteName, urls, ecommerce,
                           siteSearch, searchKeywordParameters ,
                           searchCategoryParameters , excludedIps ,
                           excludedQueryParameters , timezone,
                           currency , group, startDate, excludedUserAgents,keepURLFragments ) as t -> methodCmdImpl t + makeParams2 [|("siteName",siteName);("urls",urls)|]
                                                                                                                          +  (if (ecommerce.IsSome) then makeParam("ecommerce",ecommerce.Value) else "" )
                                                                                                                          +  (if (siteSearch.IsSome) then makeParam("siteSearch",siteSearch.Value) else "" )
                                                                                                                          +  (if (searchKeywordParameters.IsSome) then makeParam("searchKeywordParameters",searchKeywordParameters.Value) else "" )
                                                                                                                          +  (if (searchCategoryParameters.IsSome) then makeParam("searchCategoryParameters",searchCategoryParameters.Value) else "" )
                                                                                                                          +  (if (excludedIps.IsSome) then makeParam("excludedIps",excludedIps.Value) else "" )
                                                                                                                          +  (if (excludedQueryParameters.IsSome) then makeParam("excludedQueryParameters",excludedQueryParameters.Value) else "" )
                                                                                                                          +  (if (timezone.IsSome) then makeParam("timezone",timezone.Value) else "" )
                                                                                                                          +  (if (currency.IsSome) then makeParam("currency",currency.Value) else "" )
                                                                                                                          +  (if (group.IsSome) then makeParam("group",group.Value) else "" )
                                                                                                                          +  (if (startDate.IsSome) then makeParam("startDate",startDate.Value) else "" )
                                                                                                                          +  (if (excludedUserAgents.IsSome) then makeParam("excludedUserAgents",excludedUserAgents.Value) else "" )
                                                                                                                          +  (if (keepURLFragments.IsSome) then makeParam("keepURLFragments",keepURLFragments.Value) else makeParam("keepURLFragments",0) )
                | AddSiteAliasUrls(sid, urls)  as t -> methodCmdImpl t  + (sid:>ApiParameter).Command + makeParam("urls",urls)
                | GetIpsForRange (ipRange) as t -> methodCmdImpl t + makeParam("ipRange",ipRange)
                | SetGlobalExcludedIps (excludedIps)  as t -> methodCmdImpl t + makeParam("excludedIps",excludedIps)
                | SetGlobalSearchParameters (searchKeywordParameters, searchCategoryParameters) as t -> methodCmdImpl t + makeParams2 [|("searchKeywordParameters",searchKeywordParameters);("searchCategoryParameters",searchCategoryParameters)|]
                | SetGlobalExcludedUserAgents (excludedUserAgents) as t -> methodCmdImpl t + makeParam("excludedUserAgents",excludedUserAgents)
                | SetSiteSpecificUserAgentExcludeEnabled (enabled) as t -> methodCmdImpl t + makeParam("enabled",enabled)
                | SetGlobalExcludedQueryParameters (excludedQueryParameters)  as t -> methodCmdImpl t + makeParam("excludedQueryParameters",excludedQueryParameters)
                | SetDefaultCurrency (defaultCurrency)  as t -> methodCmdImpl t + makeParam("defaultCurrency",defaultCurrency)
                | GetPatternMatchSites (pattern)  as t -> methodCmdImpl t + makeParam("pattern",pattern)
                | UpdateSite(sid, siteName, urls, ecommerce ,
                                 siteSearch, searchKeywordParameters,
                                  searchCategoryParameters, excludedIps,
                                  excludedQueryParameters, timezone , 
                                  currency , group , startDate , excludedUserAgents,keepURLFragments)  as t -> methodCmdImpl t + (sid:>ApiParameter).Command + makeParam("siteName",siteName)
                                                                                                                                  +  (if (urls.IsSome) then makeParam("urls",urls.Value) else "" )
                                                                                                                                  +  (if (ecommerce.IsSome) then makeParam("ecommerce",ecommerce.Value) else "" )
                                                                                                                                  +  (if (siteSearch.IsSome) then makeParam("siteSearch",siteSearch.Value) else "" )
                                                                                                                                  +  (if (searchKeywordParameters.IsSome) then makeParam("searchKeywordParameters",searchKeywordParameters.Value) else "" )
                                                                                                                                  +  (if (searchCategoryParameters.IsSome) then makeParam("searchCategoryParameters",searchCategoryParameters.Value) else "" )
                                                                                                                                  +  (if (excludedIps.IsSome) then makeParam("excludedIps",excludedIps.Value) else "" )
                                                                                                                                  +  (if (excludedQueryParameters.IsSome) then makeParam("excludedQueryParameters",excludedQueryParameters.Value) else "" )
                                                                                                                                  +  (if (timezone.IsSome) then makeParam("timezone",timezone.Value) else "" )
                                                                                                                                  +  (if (currency.IsSome) then makeParam("currency",currency.Value) else "" )
                                                                                                                                  +  (if (group.IsSome) then makeParam("group",group.Value) else "" )
                                                                                                                                  +  (if (startDate.IsSome) then makeParam("startDate",startDate.Value) else "" )
                                                                                                                                  +  (if (excludedUserAgents.IsSome) then makeParam("excludedUserAgents",excludedUserAgents.Value) else "" )
                                                                                                                                  +  (if (keepURLFragments.IsSome) then makeParam("keepURLFragments",keepURLFragments.Value) else makeParam("keepURLFragments",0) )
                | _ as t -> methodCmdImpl t 

and AnnotationsMethod =
    | Add  of SiteId * DateTime * string * string option//(idSite, date, note, starred = '0') 
    | Save of SiteId * string * DateTime option * string option * string option//(idSite, idNote, date = '', note = '', starred = '') 
    | Delete of SiteId * string //(idSite, idNote) 
    | Get of SiteId * string //(idSite, idNote) 
    | GetAll of SiteId * TimeSlice * string option//(idSite, date = '', period = 'day', lastN = '')  	 
    | GetAnnotationCountForDates of SiteId * TimeSlice * string option * string option//(idSite, date, period, lastN = '', getAnnotationText = '')  	 
    interface ApiMethod with
        member this.Command =
                match this with
                | Add  (sid, date, note, starred )  as t -> methodCmdImpl t + (sid:>ApiParameter).Command
                                                                            + makeParam("date",date.ToString("yyyy-MM-dd"))
                                                                             + makeParam("note",note)
                                                                              +  (if (starred.IsSome) then makeParam("starred",starred.Value) else makeParam("starred",0) )
                | Save(sid, idNote, date , note, starred ) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + makeParam("idNote",idNote)
                                                                            +  (if (date.IsSome) then makeParam("date",date.Value.ToString("yyyy-MM-dd")) else "")
                                                                             +  (if (note.IsSome) then makeParam("note",note.Value) else "" )
                                                                              +  (if (starred.IsSome) then makeParam("starred",starred.Value) else makeParam("starred",0) )
                | Get(sid, idNote) | Delete(sid, idNote) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + makeParam("idNote",idNote)
                | GetAll(sid, ts, lastN) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                     +  (if (lastN.IsSome) then makeParam("lastN",lastN.Value) else "" )
                | GetAnnotationCountForDates(sid, ts, lastN,getAnnotationText) as t -> methodCmdImpl t + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                                                                                                        +  (if (lastN.IsSome) then makeParam("lastN",lastN.Value) else "" )
                                                                                                        +  (if (getAnnotationText.IsSome) then makeParam("getAnnotationText",getAnnotationText.Value) else "" )