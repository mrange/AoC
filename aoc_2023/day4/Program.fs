open System.IO

let rows_ = 
  [|
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  |]
let rows = File.ReadAllLines "input.txt"

type ScratchCardResult = ScratchCardResult of int*int list*int list

module Result =
  open FParsec
  open System.Linq

  let pno = spaces1 >>. pint32
  let presult : Parser<_, unit> =
    parse {
      do! skipString "Card"
      let! no             = pno
      do! skipString ":"
      let! winningNumbers = many1Till pno (skipString " |")
      let! myNumbers      = many1 pno
      return ScratchCardResult (no, winningNumbers, myNumbers)
    }

  let parseResult s = 
    match run presult s with
    | Success (g, _, _) -> g
    | Failure (m, _, _) -> failwithf "%s" m

  let score (ScratchCardResult (no, winningNumbers, myNumbers)) =
    let c = winningNumbers.Intersect(myNumbers).Count()
    if c > 0 then
      pown 2 (c - 1)
    else
      0
    
let results = 
  rows 
    |> Array.map Result.parseResult
    |> Array.map Result.score
    |> Array.sum

printfn "Results: %A" results
