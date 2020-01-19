// You are given the following information, but you may prefer to do some research for yourself.

//     1 Jan 1900 was a Monday.
//     Thirty days has September,
//     April, June and November.
//     All the rest have thirty-one,
//     Saving February alone,
//     Which has twenty-eight, rain or shine.
//     And on leap years, twenty-nine.
//     A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

type Day = | Mon | Tue | Wed | Thu | Fri | Sat | Sun
type Month = | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
type Date = { Day: Day; Month: Month; Year: int }

let days = [| Mon; Tue; Wed; Thu; Fri; Sat; Sun |]
let addDays day numberOfDays =
    let index =
        days
        |> Array.findIndex (fun d -> d = day)
    let newIndex = (index + numberOfDays) % days.Length
    days.[newIndex]

let isLeapYear year =
    let by4 = year % 4 = 0
    if not by4 then
        false
    else
        let by100 = year % 100 = 0
        if not by100 then
            true
        else
            year % 400 = 0

let daysInMonth month year =
    match month with
    | Apr | Sep | Jun | Nov -> 30
    | Feb -> if isLeapYear year then 29 else 28
    | _ -> 31

let mapNextMonths =
    [
        (Jan, Feb); (Feb, Mar); (Mar, Apr); (Apr, May); (May, Jun); (Jun, Jul);
        (Jul, Aug); (Aug, Sep); (Sep, Oct); (Oct, Nov); (Nov, Dec); (Dec, Jan)
    ]
    |> Map.ofList

let nextMonth month =
    mapNextMonths.TryFind month |> Option.get

let nextFirstOfTheMonth date =
    let (day, month, year) = (date.Day, date.Month, date.Year)
    let daysInCurrentMonth = daysInMonth month year
    
    let nextDay = addDays day daysInCurrentMonth
    let nextMonth = nextMonth month
    let nextYear = if month = Dec then year + 1 else year
    { Day = nextDay; Month = nextMonth; Year = nextYear }

let dates =
    Seq.unfold (fun date ->
        let nextDate = nextFirstOfTheMonth date
        if nextDate.Year = 2001 then
            None
        else
            Some(date, nextDate)) { Day = Mon; Month = Jan; Year = 1900 }

[<EntryPoint>]
let main argv =
    dates
    |> Seq.skipWhile (fun d -> d.Year = 1900)
    |> Seq.filter (fun d -> d.Day = Sun)
    |> Seq.length
    |> printfn "%i"

    0