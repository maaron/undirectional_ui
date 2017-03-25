
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

let button = Button()

let window = Window(Content = button)
window.Show()
button.SizeChanged.Add (fun e -> printf "%O -> %O\n" e.PreviousSize e.NewSize)

button.Width <- 20.0

let f x = x + 1

type Foo =
    static member f (x) = x + 1

Object.ReferenceEquals (Foo.f, Foo.f)
(f :> obj).Equals(f)

type MyDelegate = delegate of int -> int
let d = MyDelegate (Foo.f)

Object.ReferenceEquals(d, d)

type Bar = { a: int ref }

let b1 = { a = ref 1 }
let b2 = { a = ref 2 }

obj.ReferenceEquals(b1, b2)

let p1 = (b1, b2)
let p2 = (b1, b1)

obj.ReferenceEquals(fst p1, snd p2)