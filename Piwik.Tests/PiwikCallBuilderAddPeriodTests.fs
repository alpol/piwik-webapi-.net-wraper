module AddPeriodTests

open System
open Xunit
open FsUnit.Xunit
open PiwikApiParameter
open PiwikCallBuilder

let apiUri ="test.com"
let contains (testFor:string) (result:string)= result.Contains(testFor)

[<Fact>]
let ``addPeriod Date(DateTime.Now, Day)`` () =
    let res = start "test.com" |> addPeriod(Date(DateTime.Now, Day)) 
    res |> contains "&date=" |> should be True
    res |> contains "&period=" |> should be True