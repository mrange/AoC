open System
open System.IO
open System.Text

let input_ = 
  [|
    "0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"
  |]

let input = File.ReadAllLines "input.txt"

let parse (s : string) =
  s.Split (' ', StringSplitOptions.TrimEntries|||StringSplitOptions.RemoveEmptyEntries)
  |> Array.map int64

let diff vs =
  vs 
  |> Array.pairwise 
  |> Array.map (fun (x, y) -> y - x)

let interpolateLast diff history = (history |> Array.last) + diff

let interpolateFirst diff history = (history |> Array.head) - diff

[<TailCall>]
let rec solve interpolate history =
  let vs = history |> List.head
  let ds = diff vs
  if ds |> Array.forall ((=) 0L) then
    history |> List.fold interpolate 0L
  else
    solve interpolate (ds::history)

let lines = input |> Array.map parse

do
  let interpolated = lines |> Array.map (fun x -> solve interpolateLast [x]) 

  let sum = interpolated |> Array.sum

  printfn "Result 1: %d" sum

do
  let interpolated = lines |> Array.map (fun x -> solve interpolateFirst [x]) 

  let sum = interpolated |> Array.sum

  printfn "Result 2: %d" sum