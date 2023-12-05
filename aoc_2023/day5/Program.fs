open System
open System.IO

let data = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

type MapRange =
  {
    Destination : int64
    Source      : int64
    Length      : int64
  }
  member x.MapValue s =
    if s >= x.Source && s < (x.Source + x.Length) then
      Some (s - x.Source + x.Destination)
    else
      None

type Map =
  {
    Source      : string
    Destination : string
    MapRanges   : MapRange array
  }
  member x.MapValue s =
    match x.MapRanges |> Array.tryPick (fun x -> x.MapValue s) with
    | Some d  -> d
    | None    -> s

type Input =
  {
    Seeds : int64 array
    Maps  : Map array
  }

module Parse =
  open System.Text.RegularExpressions
  
  let re s = Regex(s, RegexOptions.Compiled|||RegexOptions.CultureInvariant)

  let reSeeds     = re"^seeds:(?<seeds>(\s+(\d+))*)$"
  let reRange     = re"^(?<destination>\d+) (?<source>\d+) (?<length>\d+$)"
  let reMapHeader = re"^(?<from>[a-z]+)-to-(?<to>[a-z]+) map:$"

  let (|PIsNull|_|) s : unit option =
    if isNull s then
      Some ()
    else
      None

  let (|PEmpty|_|) s : unit option =
    if String.IsNullOrWhiteSpace s then
      Some ()
    else
      None

  let (|PSeeds|_|) s : int64 array option =
    let m = reSeeds.Match s
    if m.Success then
      let seeds = 
        m
          .Groups.["seeds"]
          .Value
          .Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
      seeds |> Array.map int64 |> Some
    else
      None

  let (|PMapRange|_|) s : MapRange option =
    let m = reRange.Match s
    if m.Success then
      {
        Destination = int64 m.Groups.["destination"].Value
        Source      = int64 m.Groups.["source"].Value
        Length      = int64 m.Groups.["length"].Value
      } |> Some
    else
      None

  let (|PMapHeader|_|) s : (string*string) option =
    let m = reMapHeader.Match s
    if m.Success then
      (m.Groups.["from"].Value, m.Groups.["to"].Value) |> Some
    else
      None

  let mkMap header ranges maps =
    match header with
    | None        -> maps
    | Some (f,t)  ->
      {
        Source      = f
        Destination = t
        MapRanges   = ranges |> List.rev |> List.toArray
      }::maps

  module Loops =
    [<TailCall>]
    let rec input 
      (sr : StringReader)
      seeds 
      maps 
      header 
      ranges =
      match sr.ReadLine () with
      | PIsNull         ->
        let maps = mkMap header ranges maps
        {
          Seeds = seeds
          Maps  = maps |> List.rev |> List.toArray
        } |> Some
      | PEmpty          -> input sr seeds maps header ranges
      | PMapRange range -> input sr seeds maps header (range::ranges)
      | PMapHeader (f,t)->
        let maps = mkMap header ranges maps
        input sr seeds maps (Some (f,t)) []
      | l               -> failwithf "Line unmatched: %s" l

    let rec initial (sr : StringReader) =
      match sr.ReadLine () with
      | PIsNull       -> None
      | PEmpty        -> initial sr
      | PSeeds seeds  -> input sr seeds [] None []
      | l             -> failwithf "Line unmatched: %s" l

  let input s : Input option =
    use sr = new StringReader (s)
    Loops.initial sr

let input = Parse.input data
printfn "%A" input
