
(*

This is an exploration into using the same idea as observable.fsx, but using just input and 
output type parameters instead of a separate output and view "output".

*)

#r "../packages/System.Reactive.Core.3.0.0/lib/net45/System.Reactive.Core.dll"
#r "../packages/System.Reactive.Interfaces.3.0.0/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/System.Reactive.Linq.3.0.0/lib/net45/System.Reactive.Linq.dll"
#r "../packages/System.Reactive.PlatformServices.3.0.0/lib/net45/System.Reactive.PlatformServices.dll"
#r "../packages/System.Reactive.Windows.Threading.3.0.0/lib/net45/System.Reactive.Windows.Threading.dll"
#r "../packages/FSharp.Control.Reactive.3.5.0/lib/net45/FSharp.Control.Reactive.dll"

open System
open FSharp.Control.Reactive
open System.Reactive.Subjects
open System.Reactive.Subjects

module Tuple =
    let mapSnd f (a, b) = (a, f b)

    let chooseSnd f (a, b) =
        match f b with
        | Some c -> Some (a, c)
        | None -> None

// Generic "signal transformer" type consisting of an input observer and an output observable.
type ST<'i, 'o> = ISubject<'i> * IObservable<'o>

let empty () =
    (Subject() :> ISubject<_>, Observable.empty)

let retn a = 
    (Subject() :> ISubject<_>, Observable.single a)

module Observable =
    /// Just like combineLatest, but takes a 2-arg function to merge the latest values from each 
    /// stream into a single-valued stream.
    let applyLatest f io = Observable.combineLatest f io |> Observable.map (fun (a, b) -> a b)

let apply (st1: ST<'i, 'a -> 'b>) (st2: ST<'i, 'a>) =
    let (i1, o1) = st1
    let (i2, o2) = st2

    i2.Subscribe(i1) |> ignore

    // This is confusing, but don't be tempted to use Observable.apply here.  I don't quite yet 
    // completely understand the difference.
    (i2, Observable.applyLatest o1 o2)

let inline (<*>) a b = apply a b

let map f (i, o) =
    (i, o |> Observable.map f)

let lmap f (st: ST<'i, 'o>) =
    let (i, o) = st
    let i' = System.Reactive.Subjects.Subject() :> ISubject<_>
    (i' |> Observable.map f).Subscribe(i) |> ignore
    (i', o)

let dimap fl fr = lmap fl >> map fr

let arr f =
    let input = System.Reactive.Subjects.Subject() :> ISubject<_>
    let output = input |> Observable.map f
    (input, output)

let first (st: ST<'i, 'o>): ST<'i * 'a, 'o * 'a> =
    let (input, output) = st
    let input' = System.Reactive.Subjects.Subject() :> ISubject<_>
    
    (input' |> Observable.map fst).Subscribe(input) |> ignore
    
    let output' = (input' |> Observable.map snd) |> Observable.combineLatest output

    (input', output')

let (>>>) (st1: ST<'a, 'b>) (st2: ST<'b, 'c>): ST<'a, 'c> =
    let (i1, o1) = st1
    let (i2, o2) = st2
    
    let o2' = System.Reactive.Linq.Observable.Create(Func<_,_>(fun obs -> 
        o2.Subscribe(obs) |> ignore
        o1.Subscribe(i2)
        ))
    
    (i1, o2')

let choose (f: 'a -> 'b option) (st: ST<'i, 'a>): ST<'i, 'b> =
    let (i, a) = st
    (i, a |> Observable.choose f)

let dupInput (st: ST<'i, 'o>): ST<'i, 'o * 'i> =
    let (i, o) = st
    (i, Observable.combineLatest o i)

let dropSnd st = map fst st

let fromChooser (f: 'i -> 'o option): ST<'i, 'o> =
    let input = System.Reactive.Subjects.Subject() :> ISubject<_>
    let output = input |> Observable.choose f
    (input, output)

type Point =
  { x: float
    y: float }
    
    with
        static member zero = { x = 0.0; y = 0.0 }
        static member square x = { x = x; y = x }
        static member add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
        static member subtract p1 p2 = { x = p1.x - p2.x; y = p1.y - p2.y }
        static member max p1 p2 = { x = max p1.x p2.x; y = max p1.y p2.y }

type Rectangle =
  { topLeft: Point
    size: Point }

type Color = int

type Brush =
    | Solid of Color

type Stroke =
  { brush: Brush
    thickness: float }

type Matrix3x2 =
  { M11: float; M21: float; M31: float
    M12: float; M22: float; M32: float }

    with
        static member Translation p =
          { M11 = 1.0; M21 = 0.0; M31 = p.x
            M12 = 0.0; M22 = 1.0; M32 = p.y }

        static member TransformPoint m p = 
          { x = m.M11 * p.x + m.M21 * p.y + m.M31
            y = m.M12 * p.x + m.M22 * p.y + m.M32 }

type Drawing = 
    | DrawEmpty
    | DrawFill of Rectangle * Brush
    | DrawRectangle of Rectangle * Stroke
    | DrawText of string
    | DrawTransformed of Matrix3x2 * Drawing
    | DrawClipped of Rectangle * Drawing
    | DrawGroup of Drawing list

type View =
  { size: Point
    drawing: Drawing }

    with
        static member pad width { size = size; drawing = drawing } = 
          { size = { x = size.x + width * 2.0; y = size.y + width * 2.0 }
            drawing = DrawTransformed (Matrix3x2.Translation { x = width; y = width }, drawing) }

        static member margin newsize width { size = size; drawing = drawing } = 
          { size = newsize
            drawing = DrawTransformed (Matrix3x2.Translation (Point.square width), drawing) }

        static member overlay bottom top =
          { size = Point.max bottom.size top.size
            drawing = DrawGroup [bottom.drawing; top.drawing] }

let view size drawing = { size = size; drawing = drawing }

// A View that is dependent on another view
type ViewT = ST<View, View>


type Mouse =
  { location: Point
    leftButton: bool
    rightButton: bool }

type UserEvent =
    | MouseEvent of Mouse option
    | WindowEvent of Point

type UserT<'a> = ST<UserEvent, 'a>

type WindowT = ST<Point, View>

type MouseT = ST<Mouse option, View>

let mouse (user: UserT<'a>): UserT<'a * Mouse option> = 
    user 
    |> dupInput 
    |> choose (Tuple.chooseSnd (function MouseEvent e -> Some e | _ -> None))

let window (user: UserT<'a>): UserT<'a * Point> = 
    user 
    |> dupInput 
    |> choose (Tuple.chooseSnd (function WindowEvent e -> Some e | _ -> None))

let run (ui: UserT<View>) events =
    printf "Running UI\n"
    let (i, o) = ui
    o.Subscribe (fun view -> printf "UI event: %A\n" view) |> ignore
    events |> List.iter (fun e ->
        i.OnNext (e))

let chooseWindow = function WindowEvent e -> Some e | _ -> None
let chooseMouse = function MouseEvent e -> Some e | _ -> None

let drawWindow (f: Point -> View option) = fromChooser (chooseWindow >> Option.bind f)
let drawMouse (f: Mouse option -> View option) = fromChooser (chooseMouse >> Option.bind f)

let label s = 
    let size = Point.zero // TODO: Measure text size
    view size (DrawText s)

let clear brush =
    drawWindow (fun size ->
        let bounds = { topLeft = { x = 0.0; y = 0.0 }; size = size }
        Some <| view size (DrawFill (bounds, brush)))

let overlay (bottom: UserT<View>) (top: UserT<View>): UserT<View> =
    retn View.overlay <*> bottom <*> top

type ViewTransform =
  { twindow: Point -> Point
    tmouse: Point -> Point
    tview: Point -> View -> View }

let arranged transform (user: UserT<View>): UserT<View> =
    let user' =
        user
        |> lmap (function
            | MouseEvent e -> 
                e
                |> Option.map (fun e -> { e with location = transform.tmouse e.location })
                |> MouseEvent 
            | WindowEvent e ->
                WindowEvent (e |> transform.twindow))

    let window = fromChooser chooseWindow

    retn transform.tview <*> window <*> user'

let padded width ui =
    let tform = 
      { twindow = id
        tmouse = Point.subtract { x = width; y = width }
        tview = fun size view -> View.pad width view }
    arranged tform ui

let margined width ui =
    let tform = 
      { twindow = fun size -> 
            Point.subtract size (Point.square (width * 2.0))
        tmouse = Point.subtract (Point.square width)
        tview = fun size view -> View.margin size width view }
    arranged tform ui

let mouseString m =
    match m with
    | Some { location = { x = x; y = y } } -> sprintf "(%f, %f)" x y
    | None -> "no mouse"

let headered header body =
    dupInput header
    |> map (fun (view,size) -> { size with y = size.y - view.size.y }, view)

let ui = (retn (fun a b -> a)) <*> (clear (Solid 1)) <*> (clear (Solid 2))
run (ui)
  [ WindowEvent { x = 200.0; y = 400.0 } ]

run (clear (Solid 1) |> overlay (clear (Solid 2)))
  [ WindowEvent { x = 200.0; y = 400.0 } ]

run (clear (Solid 1) |> overlay (drawMouse (mouseString >> label >> Some))) 
  [ WindowEvent { x = 200.0; y = 400.0 } 
    MouseEvent <| Some
      { location = 
          { x = 12.0
            y = 21.0 }
        leftButton = false
        rightButton = false }
  ]

run 
  ( clear (Solid 1)
    |> padded 5.0)
  [ WindowEvent { x = 20.0; y = 40.0 } 
  ]

run 
  ( clear (Solid 1)
    |> margined 5.0)
  [ WindowEvent { x = 20.0; y = 40.0 } 
  ]

let (i, o) = (arr ((+) 1) >>> arr ((*) 2))
o.Subscribe(printf "output = %A\n")
i.OnNext(1)
i.OnNext(2)
i.OnNext(3)
i.OnNext(4)
i.OnNext(5)