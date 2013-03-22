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
    | Get //(idSite, period, date, segment = '', columns = '')  	 
    | GetPageUrls //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageUrlsFollowingSiteSearch //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageTitlesFollowingSiteSearch //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetEntryPageUrls //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetExitPageUrls //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageUrl //(pageUrl, idSite, period, date, segment = '') 
    | GetPageTitles //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetEntryPageTitles //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetExitPageTitles //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetPageTitle //(pageName, idSite, period, date, segment = '') 
    | GetDownloads //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetDownload //(downloadUrl, idSite, period, date, segment = '') 
    | GetOutlinks //(idSite, period, date, segment = '', expanded = '', idSubtable = '')  	 
    | GetOutlink //(outlinkUrl, idSite, period, date, segment = '') 
    | GetSiteSearchKeywords //(idSite, period, date, segment = '')  	 
    | AddPagesPerSearchColumn //(dataTable, columnToRead = 'nb_hits') 
    | GetSiteSearchNoResultKeywords //(idSite, period, date, segment = '')  	 
    | GetSiteSearchCategories //(idSite, period, date, segment = '') 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and ReferersMethod =
    | GetRefererType
    | GetKeywords
    | GetKeywordsForPageUrl
    | GetKeywordsForPageTitle
    | GetSearchEnginesFromKeywordId
    | GetSearchEngines
    | GetKeywordsFromSearchEngineId
    | GetCampaigns
    | GetKeywordsFromCampaignId
    | GetWebsites
    | GetUrlsFromWebsiteId
    | GetSocials
    | GetUrlsForSocial
    | GetNumberOfDistinctSearchEngines
    | GetNumberOfDistinctKeywords
    | GetNumberOfDistinctCampaigns
    | GetNumberOfDistinctWebsites
    | GetNumberOfDistinctWebsitesUrls
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and GoalsMethod =
    | GetGoals //(idSite) [ Example in XML, Json, Tsv (Excel) ]
    | AddGoal //(idSite, name, matchAttribute, pattern, patternType, caseSensitive = '', revenue = '', allowMultipleConversionsPerVisit = '') [ No example available ]
    | UpdateGoal //(idSite, idGoal, name, matchAttribute, pattern, patternType, caseSensitive = '', revenue = '', allowMultipleConversionsPerVisit = '') [ No example available ]
    | DeleteGoal //(idSite, idGoal) [ No example available ]
    | GetItemsSku //(idSite, period, date, abandonedCarts = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetItemsName //(idSite, period, date, abandonedCarts = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetItemsCategory //(idSite, period, date, abandonedCarts = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | Get //(idSite, period, date, segment = '', idGoal = '', columns = 'Array') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetDaysToConversion //(idSite, period, date, segment = '', idGoal = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetVisitsUntilConversion //(idSite, period, date, segment = '', idGoal = '') [ Example in XML, Json, Tsv 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and UserCountryMethod =
    | GetCountry
    | GetContinent
    | GetRegion
    | GetCity
    | GetLocationFromIP
    | GetNumberOfDistinctCountries
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 

and VisitorInterestMethod =
    | GetNumberOfVisitsPerVisitDuration
    | GetNumberOfVisitsPerPage
    | GetNumberOfVisitsByVisitCount
    | GetNumberOfVisitsByDaysSinceLast
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and VisitFrequencyMethod =
    | Get
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and ProviderMethod =
    | GetProvider
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
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
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and APIMethod =
    | GetPiwikVersion //() 
    | GetSettings //() 
    | GetDefaultMetricTranslations //() 
    | GetDefaultMetrics //() 
    | GetDefaultProcessedMetrics //() 
    | GetDefaultMetricsDocumentation //() 
    | GetSegmentsMetadata //(idSites = 'Array') 
    | GetLogoUrl //(pathOnly = '') 
    | GetHeaderLogoUrl //(pathOnly = '') 
    | GetMetadata //(idSite, apiModule, apiAction, apiParameters = 'Array', language = '', period = '', date = '', hideMetricsDoc = '', showSubtableReports = '')  	 
    | GetReportMetadata //(idSites = '', period = '', date = '', hideMetricsDoc = '', showSubtableReports = '')  	 
    | GetProcessedReport //(idSite, period, date, apiModule, apiAction, segment = '', apiParameters = '', idGoal = '', language = '', showTimer = '1', hideMetricsDoc = '', idSubtable = '', showRawMetrics = '')  	 
    | Get //(idSite, period, date, segment = '', columns = '')  	 
    | GetRowEvolution //(idSite, period, date, apiModule, apiAction, label = '', segment = '', column = '', language = '', idGoal = '', legendAppendMetric = '1', labelUseAbsoluteUrl = '1')  	 
    | GetBulkRequest //(urls) 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and SitesManagerMethod =
    | GetJavascriptTag //(idSite, piwikUrl = '') 
    | GetSitesFromGroup //(group) 
    | GetSitesGroups// () 
    | GetSiteFromId //(idSite) 
    | GetSiteUrlsFromId //(idSite) 
    | GetAllSites //() 
    | GetAllSitesId //() 
    | GetSitesIdWithVisits //(timestamp = '') 
    | GetSitesWithAdminAccess //() 
    | GetSitesWithViewAccess //() 
    | GetSitesWithAtLeastViewAccess// (limit = '') 
    | GetSitesIdWithAdminAccess //() 
    | GetSitesIdWithViewAccess //() 
    | GetSitesIdWithAtLeastViewAccess// () 
    | GetSitesIdFromSiteUrl //(url) 
    | AddSite //(siteName, urls, ecommerce = '', siteSearch = '', searchKeywordParameters = '', searchCategoryParameters = '', excludedIps = '', excludedQueryParameters = '', timezone = '', currency = '', group = '', startDate = '', excludedUserAgents = '') 
    | DeleteSite // (idSite) 
    | AddSiteAliasUrls //(idSite, urls) 
    | GetIpsForRange //(ipRange) 
    | SetGlobalExcludedIps //(excludedIps) 
    | SetGlobalSearchParameters //(searchKeywordParameters, searchCategoryParameters) 
    | GetSearchKeywordParametersGlobal // () 
    | GetSearchCategoryParametersGlobal //() 
    | GetExcludedQueryParametersGlobal //() 
    | GetExcludedUserAgentsGlobal //() 
    | SetGlobalExcludedUserAgents //(excludedUserAgents) 
    | IsSiteSpecificUserAgentExcludeEnabled //() 
    | SetSiteSpecificUserAgentExcludeEnabled //(enabled) 
    | SetGlobalExcludedQueryParameters //(excludedQueryParameters) 
    | GetExcludedIpsGlobal //() 
    | GetDefaultCurrency //() 
    | SetDefaultCurrency //(defaultCurrency) 
    | GetDefaultTimezone //() 
    | SetDefaultTimezone //(defaultTimezone) 
    | UpdateSite //(idSite, siteName, urls = '', ecommerce = '', siteSearch = '', searchKeywordParameters = '', searchCategoryParameters = '', excludedIps = '', excludedQueryParameters = '', timezone = '', currency = '', group = '', startDate = '', excludedUserAgents = '') 
    | GetCurrencyList //() 
    | GetCurrencySymbols// () 
    | GetTimezonesList //() 
    | GetUniqueSiteTimezones// () 
    | GetPatternMatchSites //(pattern) 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 

and AnnotationsMethod =
    | Add //(idSite, date, note, starred = '0') 
    | Save //(idSite, idNote, date = '', note = '', starred = '') 
    | Delete //(idSite, idNote) 
    | Get //(idSite, idNote) 
    | GetAll //(idSite, date = '', period = 'day', lastN = '')  	 
    | GetAnnotationCountForDates //(idSite, date, period, lastN = '', getAnnotationText = '')  	 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
