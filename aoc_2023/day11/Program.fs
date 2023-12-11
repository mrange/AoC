open System.IO
open System.Linq

let input = File.ReadAllLines "input.txt"

type Galaxy =
  {
    Id  : int64*int64
    X   : int64
    Y   : int64
  }

let rows    = input.Length
let columns =
  let cs = input |> Array.map _.Length |> Array.distinct
  if cs.Length = 1 then
    cs.[0]
  else
    failwithf "Different inputs widths: %A" cs

let parse () =
  let galaxies = ResizeArray<Galaxy> ()
  for y = 0 to rows - 1 do
    let r = input.[y]
    for x = 0 to columns - 1 do
      let c = r.Chars x
      if c = '#' then
        galaxies.Add {Id = (x,y); X = x; Y = y}
  galaxies.ToArray ()

let galaxies = parse ()

let allColumns  = Array.init columns int64
let allRows     = Array.init rows    int64

let notEmptyColumns = galaxies |> Array.map (fun g -> g.X) |> Array.distinct
let notEmptyRows    = galaxies |> Array.map (fun g -> g.Y) |> Array.distinct

let emptyColumns = (allColumns.Except notEmptyColumns).ToArray ()
let emptyRows    = (allRows.Except notEmptyRows).ToArray ()

let expand = 1000000L-1L

let expandX x (g : Galaxy) = if g.X > x then { g with X = g.X + expand } else g
let expandY y (g : Galaxy) = if g.Y > y then { g with Y = g.Y + expand } else g

let expand0 =
  emptyColumns
  |> Array.sortBy (fun x -> -x)
  |> Array.fold (fun s x -> s |> Array.map (expandX x)) galaxies

let expand1 =
  emptyRows
  |> Array.sortBy (fun y -> -y)
  |> Array.fold (fun s y -> s |> Array.map (expandY y)) expand0

let sum = 
  Array.allPairs expand1 expand1
  |> Array.map (fun (f, s) -> abs (f.X-s.X) + abs (f.Y-s.Y))
  |> Array.sum

printfn "%A" (sum / 2L)
