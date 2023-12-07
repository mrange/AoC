open System.IO
open System.Text

let input_ = 
  [|
    "32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"
  |]

let input = File.ReadAllLines "input.txt"

type Card =
  {
    Value : int
  }
  override x.ToString () =
    match x.Value with
    | v when v >= 2 && v <= 9  -> string v
    | 10  -> "T"
    | 11  -> "J"
    | 12  -> "Q"
    | 13  -> "K"
    | 14  -> "A"
    | v   -> sprintf "Out of range: %d" v

  static member FromChar c : Card =
    let v =
      match c with
      | d when d >= '2' && d <= '9'  -> int (d - '0')
      | 'T' -> 10
      | 'J' -> 11
      | 'Q' -> 12
      | 'K' -> 13
      | 'A' -> 14
      | _   -> failwithf "Invalid card: %A" c
    {
      Value = v
    }

type Hand = 
  | Hand of Card array

  override x.ToString () =
    let (Hand h) = x
    let sb = StringBuilder ()
    for c in h do
      sb.Append (string c) |> ignore
    string sb

let parse (s : string) =
  let ss = s.Split ' '
  let h = ss.[0]
  let b = ss.[1]

  if h.Length <> 5 then failwithf "Hand must be 5 cards: %s" s
  let hand = 
    h 
    |> Seq.map Card.FromChar 
    |> Seq.toArray 
    |> Hand
  hand , int b

let bids = input |> Array.map parse

type HandType =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind

type ClassifiedHand = ClassifiedHand of HandType*Hand

let classify (Hand h) =
  let g = 
    h
    |> Array.groupBy id
    |> Array.map (fun (_, g) -> g.Length)
    |> Array.sortBy (fun g -> -g)
  let c = 
    match (g |> Array.tryItem 0, g |> Array.tryItem 1) with
    | Some 5, _       -> FiveOfAKind
    | Some 4, _       -> FourOfAKind
    | Some 3, Some 2  -> FullHouse
    | Some 3, _       -> ThreeOfAKind
    | Some 2, Some 2  -> TwoPair
    | Some 2, _       -> OnePair
    | _     , _       -> HighCard
  ClassifiedHand (c, Hand h)

let rankedBids = 
  bids
  |> Array.map    (fun (h, b) -> classify h, b )
  |> Array.sortBy (fun (c, _) -> c)
  |> Array.mapi   (fun i (c, b) -> i + 1, c, b)

for (r, ClassifiedHand (c, h), b) in rankedBids do
  printfn "%d, %A, %s, %d" r c (string h) b

let tw =
  rankedBids
  |> Array.map (fun (r, _, b) -> int64 (r*b))
  |> Array.sum

printfn "%A" tw