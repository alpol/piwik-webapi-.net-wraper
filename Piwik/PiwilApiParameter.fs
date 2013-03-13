module PiwikApiParameter

open System

module private Helpers =
    let dateFormat = "yyyy-MM-dd"
    let getParamArg (param:string) (uri:string) =
        let ps = uri.Split([|'&'|])
        let p = Array.Find(ps, fun e -> e.StartsWith(param))
        let pa = p.Split([|'='|])
        pa.[1]


exception ParameterDuplicationException of string * string

type ApiParameter =
    abstract Command : string
    abstract Name : string

type ApiMethod =
    abstract Command : string
              
type TimeSlice =
    | Date  of DateTime * PeriodType
    | Today | Yesterday
    | Last     of int * PeriodType
    | Previous of int * PeriodType
    | Period    of DateTime * DateTime * PeriodType      
    | Range     of DateTime * DateTime
    interface ApiParameter with
        member this.Command =
            match this with
                | Date(d,p)      -> String.Format("{0}&date={1}",p.Command, d.ToString(Helpers.dateFormat))
                | Today         -> "&period=day&date=today"
                | Yesterday     -> "&period=day&date=yesterday"
                | Last(n,p)     -> String.Format("{0}&date=last{1}",p.Command,n)  
                | Previous(n,p) -> String.Format("{0}&date=previous{1}",p.Command,n)
                | Period(s,e,p) -> String.Format("{0}&date={1},{2}",p.Command,s.ToString(Helpers.dateFormat),e.ToString(Helpers.dateFormat))
                | Range (s,e)   -> String.Format("&period=range&date={0},{1}",s.ToString(Helpers.dateFormat),e.ToString(Helpers.dateFormat))
        member this.Name = "period"
            
and PeriodType = 
    | Day   
    | Week
    | Month
    | Year
    member this.Command =
        match this with
        | Day   ->  "&period=day"
        | Week  ->  "&period=week"
        | Month ->  "&period=month"
        | Year  ->  "&period=year"
        
           
type FormatType =
    | Xml | Json | Csv |Tsv |Html | Php | Rss | Original
    interface ApiParameter with
        member this.Command =
            match this with
                | Xml   ->  "&format=xml"
                | Json  ->  "&format=json"
                | Csv   ->  "&format=csv"
                | Tsv   ->  "&format=tsv"
                | Html  ->  "&format=html"
                | Php   ->  "&format=php"
                | Rss   ->  "&format=rss"
                | Original  ->  "&format=original"
        member this.Name = "format"


type SegmentOperator =
    | Equals | NotEquals | LessThanOrEqual | LessThan | GreaterThen | GreaterThenOrEqual | Contains |NotContains
    member this.Command =
        match this with
        | Equals            -> "=="
        | NotEquals         -> "!="
        | LessThan          -> "<"
        | LessThanOrEqual   -> "<="
        | GreaterThen       -> ">"
        | GreaterThenOrEqual-> ">="
        | Contains          -> "=@"
        | NotContains       -> "!@"

type SegmentLogicalOperator =
    | Or | And
    member this.Command =
        match this with
        | Or -> ","
        | And -> ";"


type SegmentDemention =
    | VisitLocationCity of SegmentOperator * string
    | VisitLocationContinent of SegmentOperator * Continents
    | VisitLocationCountry of SegmentOperator * string
    | VisitLocationLat of SegmentOperator * double
    | VisitLocationLong of SegmentOperator * double
    | VisitLocationProvider of SegmentOperator * string
    | VisitLocationRegion of SegmentOperator * string
    | CustomVariable of int * string * string
    | CustomVariablePage of int * string * string
    member this.Command =
        match this with
        |VisitLocationCity(op,var) -> String.Format("city{0}{1}", op.Command, var)
        |VisitLocationContinent(op,var) -> String.Format("continent{0}{1}", op.Command, var.Command)
        |VisitLocationCountry(op,var) -> String.Format("country{0}{1}", op.Command, var)
        |VisitLocationLat(op,var) -> String.Format("lat{0}{1}", op.Command, var)
        |VisitLocationLong(op,var) -> String.Format("long{0}{1}", op.Command, var)
        |VisitLocationProvider(op,var) -> String.Format("provider{0}{1}", op.Command, var)
        |VisitLocationRegion(op,var) -> String.Format("region{0}{1}", op.Command, var)
        |CustomVariable(cnt,name,value) -> String.Format("customVariableName{0}=={1};customVariableValue{0}=={2}",cnt,name,value)
        |CustomVariablePage(cnt,name,value) -> String.Format("customVariablePageName{0}=={1};customVariablePageValue{0}=={2}",cnt,name,value)


and Continents =
    | Europe | Asia | America | NorthAmerica | SouthAmerica | Africa | Antarctica | Oceania
    member this.Command =
        match this with
        | Europe        -> "eur"
        | Asia          -> "asi"
        | America       -> "amc"
        | NorthAmerica  -> "amn"
        | SouthAmerica  -> "ams"
        | Africa        -> "afr"
        | Antarctica    -> "ant"
        | Oceania       -> "oce"


type SegmentType =
    | Segment of SegmentDemention
    | Segments of (SegmentDemention * SegmentLogicalOperator) list * SegmentDemention
    interface ApiParameter with
        member this.Command =
            match this with
            | Segment(d)    -> String.Format(@"&{0}={1}",(this:>ApiParameter).Name, d.Command)
            | Segments(ds,d)-> (ds|> List.fold((fun s e -> s + (fst e).Command + (snd e).Command)) "&segment=" ) + d.Command
        member this.Name = "segment"
type SiteId =
    | Single    of int
    | Many      of int[]
    | All
    interface ApiParameter with
        member this.Command = 
            match this with
                | Single(id)    -> String.Format(@"&{0}={1}",(this:>ApiParameter).Name, id)
                | Many(ids)     -> @"&idSite=" + (Array.fold(fun acc e -> acc + "," + ((int)e).ToString()  ) "" ids ).Remove(0,1)
                | All           -> @"&idSite=all"
        member this.Name = "idSite"

type FilterType =
    | Offset        of int
    | Limit         of int
    | Trancate      of int
    | Pattern       of string * string
    | ExcludeLowPop of string * string
    | HideCols      of seq<string>
    | ShowCols      of seq<string>
    | RecursivePattern of string * string
    interface ApiParameter with
        member this.Command =
            match this with
                | Offset(n)             -> String.Format(@"&filter_offset={0}",n)
                | Limit(n)              -> String.Format(@"&filter_limit={0}",n)
                | Trancate(n)           -> String.Format(@"&filter_truncate={0}",n)
                | Pattern(col,pat)      -> String.Format(@"&filter_column={0}&filter_pattern={1}",col,pat)
                | ExcludeLowPop(c,v)    -> String.Format(@"&filter_excludelowpop={0}&filter_excludelowpop_value={1}",c,v)
                | HideCols(columns)     -> String.Format(@"&hideColumns={0}", (Seq.fold(fun s e -> s + e) "," columns).Remove(0,1) )
                | ShowCols(columns)     -> String.Format(@"&showColumns={0}", (Seq.fold(fun s e -> s + e) "," columns).Remove(0,1) )
                | RecursivePattern(col,pat)      -> String.Format(@"&filter_column_recursive={0}&filter_pattern_recursive={1}",col,pat)
        member this.Name =
            match this with
                | Offset(_)             -> "filter_offset"
                | Limit(_)              -> "filter_limit"
                | Trancate(_)           -> "filter_truncate"
                | Pattern(_,_)          -> "filter_column="
                | ExcludeLowPop(_,_)    -> "filter_excludelowpop"
                | HideCols(_)           -> "hideColumns"
                | ShowCols(_)           -> "showColumns"
                | RecursivePattern(_,_) -> "filter_column_recursive"

type SortType =
    | Asc of string
    | Desc of string
    interface ApiParameter with
        member this.Command =
            match this with
                | Asc(column)  ->  String.Format(@"&filter_sort_order=asc&filter_sort_column={0}",column)
                | Desc(column)  ->  String.Format(@"&filter_sort_order=desc&filter_sort_column={0}",column)
        member this.Name = "filter_sort_order"
        
type AuthTokenType =
    | AuthToken of string
    interface ApiParameter with
        member this.Command =
            match this with 
                | AuthToken(token) -> String.Format(@"&{0}={1}", (this:>ApiParameter).Name, token)    
        member this.Name = "token_auth"

type LanguageType =
    | Language of string
    interface ApiParameter with
        member this.Command =
            match this with 
                | Language(ln) -> String.Format(@"&{0}={1}", (this:>ApiParameter).Name, ln)    
        member this.Name = "language"

type LabelType =
    | Lable of string
    interface ApiParameter with
        member this.Command =
            match this with 
                | Lable(lb) -> String.Format(@"&{0}={1}", (this:>ApiParameter).Name, lb)    
        member this.Name = "label"

type ExpandedType =
    | Expanded 
    | Flat
    interface ApiParameter with
        member this.Command =
            match this with 
                | Expanded -> String.Format(@"&{0}=1", (this:>ApiParameter).Name)
                | Flat ->  String.Format(@"&{0}=1", (this:>ApiParameter).Name)   
        member this.Name =
            match this with
                | Expanded -> "expanded"
                | Flat -> "flat"


let addParam (p:#ApiParameter) (uri:string) =
    if uri.Contains(String.Format("&{0}=",p.Name)) then raise(ParameterDuplicationException(p.Name, Helpers.getParamArg p.Name uri))
    uri + p.Command

let addParameter ((param:string),(value:string)) (uri:string) =
    if uri.Contains(String.Format("&{0}=",param)) then raise(ParameterDuplicationException(param,  Helpers.getParamArg param uri))
    String.Format(@"{0}&{1}={2}", uri, param, value)

    

