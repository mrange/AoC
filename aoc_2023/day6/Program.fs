open System.Text.RegularExpressions

let input1_ =
  [|
    "Time:      7  15   30"
    "Distance:  9  40  200"
  |]
let input1 = 
  [|
    "Time:        58     81     96     76"
    "Distance:   434   1041   2219   1218"
  |]

let getDistance charge time =
  let travelTime = max (time - charge) 0L
  charge * travelTime

let getCharge distance time =
  let ht = 0.5*float time
  let i = ht*ht-float distance
  if i >= 0. then
    let si = sqrt i
    let c0 = ht-si |> ceil |> int64
    let c1 = ht+si |> floor |> int64
    Some (c0, c1)
  else
    None

  

let re s = Regex(s, RegexOptions.Compiled|||RegexOptions.CultureInvariant)
let reNumbers = re"\d+"

let races =
  let rec extractNumbers n (m : Match) =
    if m.Success then
      extractNumbers ((int64 m.Value)::n) (m.NextMatch ())
    else
      n
  let times     = extractNumbers [] (reNumbers.Match input1.[0])
  let distances = extractNumbers [] (reNumbers.Match input1.[1])
  List.zip times distances |> List.rev |> List.toArray

let waysToWin = 
  let m (t, d) = 
    match getCharge (d + 1L) t with
    | Some (c0, c1) -> c1 - c0 + 1L, Some (c0, c1, getDistance c0 t, getDistance c1 t)
    | None          -> 0L, None
  races 
  |> Array.map m

printfn "%A" waysToWin

let result = waysToWin |> Array.map fst |> Array.reduce ( * )

printfn "%A" result

let input2_  = (940200L, 71530L)
let input2  = (434104122191218L, 58819676L)

do
  let d, t = input2
  match getCharge d t with
  | Some (c0, c1) ->
    let r = c1 - c0 + 1L
    printfn "Result: %d, %d, %d" c0 c1 r
  | None ->
    printfn "No result"

module BI =
  module Loops =
    let rec sqrtBI i bottom top =
      if bottom + 1I < top then
        let mid = (bottom + top) / 2I
        let sqr = mid*mid
        if sqr = i then
          mid
        elif sqr > i then
          sqrtBI i bottom mid
        else
          sqrtBI i mid top
      else
        printfn "%A, %A, %A" (bottom*bottom) (top*top) i
        bottom
  let sqrtBI i = Loops.sqrtBI i bigint.Zero i

  let getChargeBI distance time =
    let _2 = 2I
    let _4 = 4I
    let i = time*time-_4*distance
    if i >= bigint.Zero then
      let si = sqrtBI i
      let c0, r0 = bigint.DivRem (time - si, _2)
//      let c1, r1 = bigint.DivRem (time + si, _2)
      let v1 = bigint.Divide (time + si, _2)
      let v0 = c0 + if r0.IsZero then bigint.Zero else bigint.One
//      let v1 = c1 + if r1.IsZero then bigint.Zero else bigint.One
      Some (v0, v1)
    else
      None

let input3  = (434104122191218812191221401434I, 5881967667691885I)

do
  let d, t = input3
  match BI.getChargeBI d t with
  | Some (c0, c1) ->
    let r = c1 - c0 + bigint.One
    printfn "Result: %A, %A, %A" c0 c1 r
  | None ->
    printfn "No result"
