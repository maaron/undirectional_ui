
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

module Tuple =
    let mapSnd f (a, b) = (a, f b)

    let chooseSnd f (a, b) =
        match f b with
        | Some c -> Some (a, c)
        | None -> None

// Generic "signal transformer" type consisting of an input observer and an output observable.
type ST<'i, 'o> = ISubject<'i> * IObservable<'o>

let empty () =
    (System.Reactive.Subjects.Subject() :> ISubject<_>, Observable.empty)

let map f (i, o) =
    (i, o |> Observable.map f)

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

    