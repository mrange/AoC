open System.IO
open System.Linq

let input : string array = File.ReadAllLines "input.txt"

type Galaxy =
  {
    Id  : int64*int64
    X   : int64
    Y   : int64
  }

let rows    : int = input.Length
let columns : int =
  let cs = input |> Array.map _.Length |> Array.distinct
  if cs.Length = 1 then cs.[0] else failwithf "Different inputs widths: %A" cs

let parse () : Galaxy array =
  [|
    for y = 0 to rows - 1 do
      let r = input.[y]
      for x = 0 to columns - 1 do
        let c = r.Chars x
        if c = '#' then
          yield {Id = (x,y); X = x; Y = y}
  |]

let galaxies        : Galaxy array = parse ()

let allColumns      : int64 array = Array.init columns int64
let allRows         : int64 array = Array.init rows    int64

let notEmptyColumns : int64 array = galaxies |> Array.map (fun g -> g.X) |> Array.distinct
let notEmptyRows    : int64 array = galaxies |> Array.map (fun g -> g.Y) |> Array.distinct

let emptyColumns    : int64 array = allColumns.Except notEmptyColumns |> Seq.toArray
let emptyRows       : int64 array = allRows.Except notEmptyRows |> Seq.toArray

let expandWith      : int64       = 1000000L-1L

let expandX (x : int64) (g : Galaxy) : Galaxy = 
  if g.X > x then { g with X = g.X + expandWith } else g
let expandY (y : int64) (g : Galaxy) : Galaxy = 
  if g.Y > y then { g with Y = g.Y + expandWith } else g

let expandedColumns       : Galaxy array  =
  emptyColumns
  |> Array.sortBy (fun x -> -x)
  |> Array.fold (fun s x -> s |> Array.map (expandX x)) galaxies

let expandColumnsAndRows  : Galaxy array  =
  emptyRows
  |> Array.sortBy (fun y -> -y)
  |> Array.fold (fun s y -> s |> Array.map (expandY y)) expandedColumns

let sum : int64 = 
  Array.allPairs expandColumnsAndRows expandColumnsAndRows
  |> Array.map (fun (f, s) -> abs (f.X-s.X) + abs (f.Y-s.Y))
  |> Array.sum

printfn "%A" (sum / 2L)
