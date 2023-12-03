open System.IO

let rows = File.ReadAllLines "input.txt"
let rows_ = 
  [|
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
  |]

let rowCount = rows.Length
let colCount = rows |> Array.map _.Length |> Array.max
if rows |> Array.map _.Length |> Array.min <> colCount then
  failwith "Not all rows has the same length"

type Cell =
  | Empty
  | Number  of int
  | Symbol

let cells = 
  let init x y = 
    let row = rows.[y]
    let ch  = row.[x]
    let c   = 
      match ch with
      | '.'                         -> Empty
      | n when n >= '0' && n <= '9' -> Number (int (n-'0'))
      | _                           -> Symbol
    c
  Array2D.init 
    colCount 
    rowCount 
    init

let lookUp x y =
  if x >= 0 && x < colCount && y >= 0 && y < rowCount then
    cells.[x, y]
  else
    Empty

let hasSymbol xx yy =
  let mutable hs = false
  for x = -1 to 1 do
    for y = -1 to 1 do
      if x = 0 && y = 0 then
        // Skip
        ()
      else
        match lookUp (xx + x) (yy + y) with
        | Symbol  -> hs <- true
        | _       -> ()
  hs

let results = ResizeArray ()
for y = 0 to rowCount - 1 do
  let mutable current     = 0
  let mutable nextToSymbol= false
  let complete () =
    if current > 0 && nextToSymbol then
      results.Add current
    current       <- 0
    nextToSymbol  <- false
  for x = 0 to colCount - 1 do
    let cell = cells.[x, y]
    match cell with
    | Number n  ->
      current       <- current*10+n
      nextToSymbol  <- nextToSymbol || hasSymbol x y
    | _         ->  complete ()
  complete ()

results |> Seq.sum |> printfn "Result: %d"
