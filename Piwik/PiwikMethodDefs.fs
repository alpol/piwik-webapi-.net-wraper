module PiwikMethodDefs

open PiwikApiParameter
open System

let private makeParam2 p = (p:>ApiParameter).Command 
let private makeParams p = p |> Seq.fold (fun s e -> s + (makeParam2 e) ) ""
let private makeParam (n, v) = String.Format(@"&{1}={2}",  n, v) 
let private makeParams2 p = p |> Seq.fold(fun s e -> s + (makeParam e))""
let private methodCmdImpl t = (fst( Reflection.FSharpValue.GetUnionFields(t,t.GetType()))).Name |> fun s -> s.Substring(0,1).ToLower() + s.Substring(1)
let private methodTypeCmdImpl o m s = String.Format("&{0}={1}", (o :> ApiParameter).Name, s + "." + (m :> ApiMethod).Command ) 
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
    | AddReport // (idSite, description, period, reportType, reportFormat, reports, parameters) [ No example available ]
    | UpdateReport //(idReport, idSite, description, period, reportType, reportFormat, reports, parameters) [ No example available ]
    | DeleteReport //(idReport) [ No example available ]
    | GetReports //(idSite = '', period = '', idReport = '', ifSuperUserReturnOnlySuperUserReports = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GenerateReport //(idReport, date, language = '', outputType = '', period = '', reportFormat = '', parameters = '') [ No example available ]
    | SendReport //(idReport, period = '', date = '') [ No example available ]
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and OverlayMethod =
    | GetTranslations of SiteId 
    | GetExcludedQueryParameters of SiteId 
    | GetFollowingPages  of SiteId * TimeSlice * string 
    interface ApiMethod with
        member this.Command =
                match this with
                | GetTranslations(sid) as t -> (methodCmdImpl t) +  (makeParam2 sid)
                | GetExcludedQueryParameters(sid) as t -> (methodCmdImpl t) +  (makeParam2 sid)
                | GetFollowingPages(sid,ts,url) as t -> (methodCmdImpl t) + ( makeParam2 sid) + (makeParam2 ts) + makeParam("url",url)

and MobileMessagingMethod =
    | AreSMSAPICredentialProvided //() [ Example in XML, Json, Tsv (Excel) ]
    | GetSMSProvider //() [ Example in XML, Json, Tsv (Excel) ]
    | SetSMSAPICredential //(provider, apiKey) [ No example available ]
    | AddPhoneNumber //(phoneNumber) [ No example available ]
    | SanitizePhoneNumber //(phoneNumber) [ No example available ]
    | GetCreditLeft //() [ Example in XML, Json, Tsv (Excel) ]
    | RemovePhoneNumber //(phoneNumber) [ No example available ]
    | ValidatePhoneNumber //(phoneNumber, verificationCode) [ No example available ]
    | DeleteSMSAPICredential // () [ Example in XML, Json, Tsv (Excel) ]
    | SetDelegatedManagement //(delegatedManagement) [ No example available ]
    | GetDelegatedManagement //() [ Example in XML, Json, Tsv (Excel) ]
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and LiveMethod =
    | GetCounters //(idSite, lastMinutes, segment = '') [ Example in XML, Json, Tsv (Excel) ]
    | GetLastVisitsDetails // (idSite, period, date, segment = '', filter_limit = '', maxIdVisit = '', minTimestamp = '') 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and LanguagesManagerMethod =
    | IsLanguageAvailable //(languageCode) [ Example in XML, Json, Tsv (Excel) ]
    | GetAvailableLanguages //() [ Example in XML, Json, Tsv (Excel) ]
    | GetAvailableLanguagesInfo //() [ Example in XML, Json, Tsv (Excel) ]
    | GetAvailableLanguageNames //() [ Example in XML, Json, Tsv (Excel) ]
    | GetTranslationsForLanguage //(languageCode) [ Example in XML, Json, Tsv (Excel) ]
    | GetLanguageForUser //(login) [ No example available ]
    | SetLanguageForUser //(login, languageCode) [ No example available ]
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
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
    | GetCustomVariables //(idSite, period, date, segment = '', expanded = '') 
    | GetCustomVariablesValuesFromNameId //(idSite, period, date, idSubtable, segment = '')
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and MultiSitesMethod =
    | GetOne of SiteId * TimeSlice
    | GetAll of TimeSlice
    interface ApiMethod with
        member this.Command =
                match this with
                | GetOne(sid,ts) as t -> (methodCmdImpl t) + (sid:>ApiParameter).Command + (ts:>ApiParameter).Command
                | GetAll(ts) as t -> (methodCmdImpl t) + (ts:>ApiParameter).Command

and VisitsSummaryMethod =
    | Get
    | GetVisits //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetUniqueVisitors// (idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) , RSS of the last 10 days ]
    | GetActions //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetMaxActions //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetBounceCount //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) ,	RSS of the last 10 days ]
    | GetVisitsConverted //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) , RSS of the last 10 days ]
    | GetSumVisitsLength //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv (Excel) , RSS of the last 10 days ]
    | GetSumVisitsLengthPretty //(idSite, period, date, segment = '') [ Example in XML, Json, Tsv 
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
and VisitTimeMethod =
    | GetVisitInformationPerServerTime
    | GetVisitInformationPerLocalTime
    | GetByDayOfWeek
    interface ApiMethod with
        member this.Command =
                match this with
                | _ as t -> methodCmdImpl t 
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
