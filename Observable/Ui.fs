module Ui

open Geometry
open Drawing
open View
open FSharp.Control.Reactive

type Mouse =
  { location: Point
    leftButton: bool
    rightButton: bool }

type UIEvent<'e> =
    | MouseEvent of Mouse option
    | WindowEvent of Point
    | Event of 'e

type UI<'i, 'o> =
  { input: System.IObserver<UIEvent<'i>>
    output: System.IObservable<'o>
    view: System.IObservable<View> }

let create i o v = { input = i; output = o; view = v }

let empty() =
  { input = System.Reactive.Observer.Create(System.Action<_>(ignore))
    output = Observable.single ()
    view = Observable.single (view Point.zero DrawEmpty) }

let map f ui = 
  { input = ui.input
    output = ui.output |> Observable.map f 
    view = ui.view }

let loop ui =
    ui.output.Subscribe(ui.input) |> ignore
    ui

let chooseWindowEvents input = 
    input
    |> Observable.choose (function WindowEvent e -> Some e | _ -> None)

type ViewTransform =
  { twindow: Point -> Point
    tmouse: Point -> Point
    tview: Point -> View -> View }

let selectWindow = function WindowEvent e -> Some e | _ -> None
let selectMouse = function MouseEvent e -> Some e | _ -> None
let selectEvent = function Event e -> Some e | _ -> None

let arranged transform (ui: UI<'a, 'b>): UI<'a, 'b> =
    let input = System.Reactive.Subjects.Subject ()

    let mapped =
        input
        |> Observable.map (function
            | MouseEvent e -> 
                e
                |> Option.map (fun e -> { e with location = transform.tmouse e.location })
                |> MouseEvent 
            | WindowEvent e ->
                WindowEvent (e |> transform.twindow)
            | _ as e -> e)

    mapped.Subscribe (ui.input) |> ignore

    let windowEvents = input |> Observable.choose selectWindow
    
    let output = ui.output
    let view = 
        ui.view 
        |> Observable.combineLatest windowEvents
        |> Observable.map (fun (size, view) -> transform.tview size view)

    create input output view

let drawUIEvent (f: UIEvent<'e> -> View option) =
    let input = System.Reactive.Subjects.Subject()

    let view = input |> Observable.choose f
    let output = Observable.empty
    
    create input output view

let drawMouse (f: Mouse option -> View option) = drawUIEvent (selectMouse >> Option.bind f)

let drawWindow (f: Point -> View option) = drawUIEvent (selectWindow >> Option.bind f)

let drawEvent (f: 'e -> View option) = drawUIEvent (selectEvent >> Option.bind f)

let staticView view = { empty () with view = Observable.single view }

let fill rectangle brush = 
    let bounds = 
      { x = rectangle.topLeft.x + rectangle.size.x
        y = rectangle.topLeft.y + rectangle.size.y }

    staticView <| view bounds (DrawFill (rectangle, brush))

let rectangle rectangle brush = 
    let bounds = 
      { x = rectangle.topLeft.x + rectangle.size.x
        y = rectangle.topLeft.y + rectangle.size.y }

    staticView <| view bounds (DrawRectangle (rectangle, brush))

let label s = 
    let size = Point.zero // TODO: Measure text size
    view size (DrawText s)

let clear brush =
    drawWindow (fun size ->
        let bounds = { topLeft = { x = 0.0; y = 0.0 }; size = size }
        Some <| view size (DrawFill (bounds, brush)))

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

let overlay bottom top =
    let input = System.Reactive.Subjects.Subject()

    input.Subscribe(bottom.input) |> ignore
    input.Subscribe(top.input) |> ignore

    let output = Observable.merge bottom.output top.output

    let view = 
        bottom.view
        |> Observable.combineLatest top.view
        |> Observable.map (fun (vb, vt) -> View.overlay vb vt)

    create input output view
