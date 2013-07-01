module AddMethodTests

open Xunit
open FsUnit.Xunit
open PiwikApiParameter
open PiwikMethodDefs
open PiwikCallBuilder

let apiUri ="test.com"
let contains (testFor:string) (result:string)= result.Contains(testFor)


[<Fact>]
let ``addMethod(MultiSites(GetOne(SiteId.Single(2),TimeSlice.Today,None,None))) should add &method=MultiSites.getOne&idSite=2&period=day&date=today to uri`` () =
    apiUri |> addMethod(MultiSites(GetOne(SiteId.Single(2),TimeSlice.Today,None,None))) |> contains "&method=MultiSites.getOne&idSite=2&period=day&date=today" |> should be True


[<Fact>]
let ``addMethod(MultiSites(MultiSitesMethod.GetAll(TimeSlice.Today,None,None,None))) should add &method=MultiSites.getAll&period=day&date=today to uri`` () =
    apiUri |> addMethod(MultiSites(MultiSitesMethod.GetAll(TimeSlice.Today,None,None,None))) |> contains "&method=MultiSites.getAll&period=day&date=today" |> should be True

[<Fact>]
let ``addMethod with all optional parameters (segment enhenced) should add &method=MultiSites.getOne&idSite=2&period=day&date=today&segment=city==Boston&enhanced=True to uri`` () =
    apiUri |> addMethod(MultiSites(GetOne(SiteId.Single(2),TimeSlice.Today,Some(Segment(VisitLocationCity(Equals,"Boston"))),Some(true)))) //|> System.Console.WriteLine
     |> contains "&method=MultiSites.getOne&idSite=2&period=day&date=today&segment=city==Boston&enhanced=True" |>  should be True

[<Fact>]
let ``addMethod(SitesManager(AddSiteAliasUrls(SiteId.Single(2),[|"www.test.com";"www.test2.com"|]))) should add &method=SitesManager.addSiteAliasUrls&urls[0]=www.test.com&urls[1]=www.test2.com" to uri`` () =
    let testCallResult = apiUri |> addMethod(SitesManager(AddSiteAliasUrls(SiteId.Single(2),[|"www.test.com";"www.test2.com"|])))
    testCallResult |> System.Console.WriteLine
    testCallResult |> contains "&method=SitesManager.addSiteAliasUrls&idSite=2&urls[0]=www.test.com&urls[1]=www.test2.com" |> should be True

[<Fact>]
let ``addMethod(API(GetBulkRequest([|"www.test.com";"www.test2.com"|]))) should add &method=API.getBulkRequest&urls[0]=www.test.com&urls[1]=www.test2.com" to uri`` () =
    let testCallResult = apiUri |> addMethod(API(GetBulkRequest([|"www.test.com";"www.test2.com"|])))
    testCallResult |> System.Console.WriteLine
    testCallResult |> contains "&method=API.getBulkRequest&urls[0]=www.test.com&urls[1]=www.test2.com" |> should be True


