open System.IO

let rows_ =
  [|
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  |]
let rows = File.ReadAllLines "input.txt"

type Ball = Red|Green|Blue
type GameSet = GameSet of (int*Ball) list
type Game = Game of int*GameSet list

module Game_ =
  open FParsec

  let MaxRed    = 12
  let MaxGreen  = 13
  let MaxBlue   = 14

  let pball = 
    choice
      [
        skipString "red"   >>% Red
        skipString "green" >>% Green
        skipString "blue"  >>% Blue
      ]
  let ppick =
    pint32 .>> skipChar ' ' .>>. pball
  let pset = sepBy1 ppick (skipString ", ") |>> GameSet
  let psets= sepBy1 pset  (skipString "; ")
  let pgame : Parser<Game, unit> =
    parse {
      do!   skipString "Game "
      let!  no = pint32
      do!   skipString ": "
      let!  set = psets
      return Game (no, set)
    }
  let parseGame s = 
    match run pgame s with
    | Success (g, _, _) -> g
    | Failure (m, _, _) -> failwithf "%s" m

  module Loops = 
    [<TailCall>] 
    let rec invalidSet r g b picks =
      match picks with
      | []    ->
        false
        || r > MaxRed
        || g > MaxGreen
        || b > MaxBlue
          
      | (v, ball)::xs ->
        match ball with
        | Red   -> invalidSet (r + v) g       b       xs
        | Green -> invalidSet r       (g + v) b       xs
        | Blue  -> invalidSet r       g       (b + v) xs

  let checkGame s =
    let (Game (no, sets)) = parseGame s
    let invalidSet (GameSet picks) = Loops.invalidSet 0 0 0 picks
    let invalid = 
      sets 
      |> List.exists invalidSet
    if invalid then 
//      printfn "%s" s
      0 
    else 
      no
        

rows 
  |> Array.sumBy Game_.checkGame 
  |> printfn "Result: %d"
