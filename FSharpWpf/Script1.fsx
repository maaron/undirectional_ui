
#I __SOURCE_DIRECTORY__
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "WindowsBase.dll"

open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Controls.Primitives

let style = new Style();
// Triggers, Resources, TargetType, Setters maybe aren't necessary?

// This seems to be necessary to at least have "content" and "children" properties, but what 
// methods must this have?  Another possibility is to encode all controls as union cases, but then 
// it's not "externally" extensible.  Perhaps a method that performs update given a previous
// ControlD value and a "live" Control?
type ControlD = interface end

type ButtonD<'Msg> =
  { width: float
    height: float
    content: ControlD

    click: RoutedEvent -> 'Msg
  }

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
