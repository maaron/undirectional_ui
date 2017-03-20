
open System.IO

[<CustomEquality; NoComparison>]
type ExternalCmd<'d, 'e when 'd : equality> =
  { data: 'd
    run: ('e -> unit) -> unit 
  }

    override x.GetHashCode () = x.data.GetHashCode()
    
    override x.Equals (o) =
        match o with
        | :? ExternalCmd<'d, 'e> as dc -> Unchecked.equals dc.data x.data
        | _ -> false

type InternalCmd<'d, 'e> = 
  { machineId: int
    request: 'd
    tagger: 'd -> 'e }

type Cmd<'d, 'e when 'd : equality> =
  | Internal of InternalCmd<'d, 'e>
  | External of ExternalCmd<'d, 'e>
  | Batch of Cmd<'d, 'e> list

let dc0 = { data = 123; run = fun k -> k 123 }

printf "%A\n" dc0

let b = dc0 = dc0

module Cmd =
    let create d f = { data = d; run = f }

type FileCommand =
  | Open of string
  | Close of StreamReader

let openFile path = 
    Cmd.create (Open path) <| fun k -> k (System.IO.StreamReader (path))

let closeFile stream =
    Cmd.create (Close stream) <| fun k -> stream.Dispose() |> k

openFile "C:\\asdf.txt"

(* Another way is to make Cmd an interface *)

type ICmd<'e> =
    abstract member run: ('e -> unit) -> unit

module ICmd = 
    let create d f = 
      { new ICmd<_> with
            member this.run (action) = f action }