open System
open System.Buffers
open System.IO

let rows = File.ReadAllLines "input.txt"

let digits = 
  Array.init 10 (fun i -> '0' + char i)
  |> SearchValues.Create 


let rec sum s i =
  if i < rows.Length then
    let n = i + 1
    let r = rows.[i].AsSpan ()
    let f = r.IndexOfAny digits
    let l = r.LastIndexOfAny digits


    if f >= 0 && l >= 0 then
      let v = (int64 (r.[f]-'0'))*10L + (int64 (r.[l]-'0'))
      sum (s + v) n
    else
      sum s n
  else
    s

sum 0L 0 |> printfn "Result: %d"