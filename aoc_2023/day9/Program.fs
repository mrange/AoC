﻿open System
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

[<TailCall>]
let rec interpolateLast (diff : int64) history =
  match history with
  | []    -> diff
  | x::xs ->
    let diff = (x |> Array.last) + diff
    interpolateLast diff xs

[<TailCall>]
let rec interpolateFirst (diff : int64) history =
  match history with
  | []    -> diff
  | x::xs ->
    let diff = (x |> Array.head) - diff
    interpolateFirst diff xs

[<TailCall>]
let rec solve interpolate history =
  let vs = history |> List.head
  let ds = diff vs
  if ds |> Array.forall (fun x -> x = 0L) then
    interpolate 0L history
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