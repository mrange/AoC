open System.IO
open System.Text.RegularExpressions

let re s = Regex(s, RegexOptions.Compiled|||RegexOptions.CultureInvariant)
let reInstruction = re@"(L|R)"
let reNode = re@"^(?<name>\w{3}) = \((?<left>\w{3}), (?<right>\w{3})\)$"

let input_ = 
  [|
    "RL"
    ""
    "AAA = (BBB, CCC)"
    "BBB = (DDD, EEE)"
    "CCC = (ZZZ, GGG)"
    "DDD = (DDD, DDD)"
    "EEE = (EEE, EEE)"
    "GGG = (GGG, GGG)"
    "ZZZ = (ZZZ, ZZZ)"
  |]

let input__ = 
  [|
    "LLR"
    ""
    "AAA = (BBB, BBB)"
    "BBB = (AAA, ZZZ)"
    "ZZZ = (ZZZ, ZZZ)"
  |]

let input = File.ReadAllLines "input.txt"

type Instruction = Left|Right

type Node =
  {
    Name  : string
    Left  : string
    Right : string
  }

module Loops =
  [<TailCall>]
  let rec parseInstructions (ra : Instruction ResizeArray) (m : Match) =
    if m.Success then
      ra.Add (if m.Value = "L" then Left else Right)
      parseInstructions ra (m.NextMatch ())
    else
      ra.ToArray ()
  

let parseInstructions s =
  let ra = ResizeArray ()
  let m = reInstruction.Match s
  Loops.parseInstructions ra m

let parseNode s : Node =
  let m = reNode.Match s
  if m.Success then
    {
      Name  = m.Groups.["name"].Value
      Left  = m.Groups.["left"].Value
      Right = m.Groups.["right"].Value
    }
  else
    failwithf "Not a valid node: %s" s

let instructions = parseInstructions input.[0]
let nodes =
  input
  |> Array.skip 1
  |> Array.filter (fun s -> s.Length > 0)
  |> Array.map (fun s -> let n = parseNode s in n.Name, n)
  |> Map.ofArray

let rec solve c s =
  let i = instructions.[s%instructions.Length]
  let n = nodes |> Map.find c
  if n.Name = "ZZZ" then 
    s
  else
    let nc = match i with Left -> n.Left | _ -> n.Right
    let ns = s + 1
    solve nc ns

let steps = solve "AAA" 0

printfn "Steps: %d" steps