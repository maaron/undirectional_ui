
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "../packages/System.Reactive.Core.3.0.0/lib/net45/System.Reactive.Core.dll"
#r "../packages/System.Reactive.Interfaces.3.0.0/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/System.Reactive.Linq.3.0.0/lib/net45/System.Reactive.Linq.dll"
#r "../packages/System.Reactive.PlatformServices.3.0.0/lib/net45/System.Reactive.PlatformServices.dll"
#r "../packages/System.Reactive.Windows.Threading.3.0.0/lib/net45/System.Reactive.Windows.Threading.dll"
#r "../packages/FSharp.Control.Reactive.3.5.0/lib/net45/FSharp.Control.Reactive.dll"

open FSharp.Control.Reactive

type Size = 
  { width: float
    height: float }

    with
        static member zero = { width = 0.0; height = 0.0 }

type Point =
  { x: float
    y: float }
    
    with
        static member zero = { x = 0.0; y = 0.0 }

type Rectangle =
  { topLeft: Point
    size: Size }

type Color = int

type Brush =
    | Solid of Color

type Stroke =
  { brush: Brush
    thickness: float }

type Matrix3x2 = class end

type Drawing = 
    | DrawEmpty
    | DrawFill of Rectangle * Brush
    | DrawRectangle of Rectangle * Stroke
    | DrawText of string
    | DrawTransformed of Matrix3x2 * Drawing
    | DrawClipped of Matrix3x2 * Drawing
    | DrawGroup of Drawing list

type View =
  { size: Size
    drawing: Drawing }

let view size drawing = { size = size; drawing = drawing }

type Mouse =
  { location: Point
    leftButton: bool
    rightButton: bool }

type UIEvent<'e> =
    | MouseEvent of Mouse
    | WindowEvent of Size
    | Event of 'e

type UI<'e, 'c, 'a> =
  { input: System.IObserver<UIEvent<'e>>
    output: System.IObservable<'a>
    commands: System.IObservable<'c>
    view: System.IObservable<View> }

let empty() =
  { input = System.Reactive.Observer.Create(System.Action<_>(ignore))
    output = Observable.single ()
    commands = Observable.empty
    view = Observable.single (view { width = 0.0; height = 0.0 } DrawEmpty) }

let map f ui = 
  { input = ui.input
    output = ui.output |> Observable.map f 
    commands = ui.commands 
    view = ui.view }

let bindView f ui =
  { input = ui.input
    output = ui.output
    commands = ui.commands
    view = ui.view |> Observable.merge (ui.output |> Observable.map f) }

let chooseEvent f ui =
    let input = System.Reactive.Subjects.Subject()
    
    let output = 
        input 
        |> Observable.choose f
        |> Observable.zip ui.output
    
    input.Subscribe (ui.input) |> ignore
    
    { input = input :> System.IObserver<_>
      output = output
      commands = ui.commands
      view = ui.view }

let mouse (ui: UI<'e, 'c, 'a>): UI<'e, 'c, 'a * Mouse> =
    chooseEvent (function MouseEvent e -> Some e | _ -> None) ui

let window (ui: UI<'e, 'c, 'a>): UI<'e, 'c, 'a * Size> =
    chooseEvent (function WindowEvent e -> Some e | _ -> None) ui

let staticView view = { empty () with view = Observable.single view }

let fill rectangle brush = 
    let bounds = 
      { width = rectangle.topLeft.x + rectangle.size.width
        height = rectangle.topLeft.y + rectangle.size.height }

    staticView <| view bounds (DrawFill (rectangle, brush))

let rectangle rectangle brush = 
    let bounds = 
      { width = rectangle.topLeft.x + rectangle.size.width
        height = rectangle.topLeft.y + rectangle.size.height }

    staticView <| view bounds (DrawRectangle (rectangle, brush))

let clear brush =
    empty ()
    |> window
    |> bindView (fun (_, size) ->
        let bounds = { topLeft = { x = 0.0; y = 0.0 }; size = size }
        view size (DrawFill (bounds, brush)))
    |> map ignore

let myui: UI<unit, unit, _> = 
    clear (Solid 123)
    |> mouse
    |> bindView (fun (_, m) -> 
        view Size.zero (DrawText <| string m.location))

let run ui events =
    ui.view.Subscribe (fun view -> printf "%A\n" view) |> ignore
    events |> List.iter (fun e ->
        ui.input.OnNext (e))

run myui 
  [ WindowEvent { width = 200.0; height = 400.0 }
    MouseEvent
      { location = { x = 1.0; y = 10.0 }
        leftButton = false
        rightButton = false } ]

