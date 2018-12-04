// https://adventofcode.com/2018/day/4 in F#.
// Try it here: https://tio.run/#fs-core

open System
open System.Text.RegularExpressions
open System.Collections.Generic

// A helper to update a Dictionary with a function and some default value.
let update (dict: Dictionary<'K, 'V>) (key: 'K) (updater: 'V -> 'V) (defaultValue: 'V): unit =
    dict.[key] <- updater (if dict.ContainsKey key then dict.[key] else defaultValue)

type GuardID = int
type Event =
    | BeginShift of GuardID * DateTime
    | Sleep of DateTime
    | Wake of DateTime

let timeOf: Event -> DateTime = function
    | BeginShift (_, t) -> t
    | Wake t            -> t
    | Sleep t           -> t

// Parse a single line of the input.
let parseEvent (line: string): Event =
    let m    = Regex.Match(line, @"\[(....-..-.. ..:..)\] (.*)")
    let time = DateTime.Parse m.Groups.[1].Value
    match m.Groups.[2].Value with
    | "falls asleep" -> Sleep time
    | "wakes up"     -> Wake time
    | body           -> let guardID = int(Regex.Match(body, @"\d+").Value)
                        BeginShift (guardID, time)

// Get events and sort them chronologically.
let events =
    Seq.initInfinite (fun _ -> System.Console.In.ReadLine())
    |> Seq.takeWhile ((<>) null)
    |> Seq.map parseEvent
    |> Seq.sortBy timeOf

// We'll write some stateful code to analyze the input.
type MinuteAsleep = int
let mutable currentGuard : GuardID = -1
let mutable sleepStart : Option<DateTime> = None
let mutable totalSleep = new Dictionary<GuardID, int>()
let mutable sleepSchedule = new Dictionary<GuardID, Dictionary<MinuteAsleep, int>>()

for event in events do
    match event with
    | BeginShift (g, _) ->
        currentGuard <- g
        sleepStart <- None  // Guard is awake at start of shift
    | Sleep t ->
        sleepStart <- Some(t)
    | Wake tEnd ->
        // Guh…? How long was I out for?
        let tStart = Option.get sleepStart
        sleepStart <- None
        let minutes = tEnd.Minute - tStart.Minute

        // Oh no, this nap will count toward my running total…
        update totalSleep currentGuard ((+) minutes) 0

        // *And* to the boss's schedule of which minutes I've been dozing off on most…
        update sleepSchedule currentGuard id (new Dictionary<MinuteAsleep, int>())
        for m in { tStart.Minute .. tEnd.Minute - 1 } do
            update sleepSchedule.[currentGuard] m ((+) 1) 0

// Strategy 1: get the overall sleepiest guard, and then their sleepiest minute.
let maxByValue d = Seq.maxBy (fun (KeyValue(_, v)) -> v) d
let sleepiestGuard = (maxByValue totalSleep).Key
let sleepiestMinute = (maxByValue sleepSchedule.[sleepiestGuard]).Key

printfn "%d" (sleepiestGuard * sleepiestMinute)

// Strategy 2: find the most frequent asleep (guard, minute) pair.
let (reliableGuard, reliableMinute) =
    sleepSchedule
    |> Seq.map (fun (KeyValue(guard, dict)) ->
        // From each guard's schedule, get their sleepiest minute and its frequency.
        let (KeyValue (minute, freq)) = maxByValue dict
        ((guard, minute), freq)
    )
    |> Seq.maxBy (fun (_, freq) -> freq) |> fst

printfn "%d" (reliableGuard * reliableMinute)
