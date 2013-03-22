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

