
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "WindowsBase.dll"
#r "../packages/System.Reactive.Core.3.0.0/lib/net45/System.Reactive.Core.dll"
#r "../packages/System.Reactive.Interfaces.3.0.0/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/System.Reactive.Linq.3.0.0/lib/net45/System.Reactive.Linq.dll"
#r "../packages/System.Reactive.PlatformServices.3.0.0/lib/net45/System.Reactive.PlatformServices.dll"
#r "../packages/System.Reactive.Windows.Threading.3.0.0/lib/net45/System.Reactive.Windows.Threading.dll"
#r "../packages/FSharp.Control.Reactive.3.5.0/lib/net45/FSharp.Control.Reactive.dll"

open FSharp.Control.Reactive
open System.Numerics

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

module UI =
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

    UI.create input output view

let drawUIEvent (f: UIEvent<'e> -> View option) =
    let input = System.Reactive.Subjects.Subject()

    let view = input |> Observable.choose f
    let output = Observable.empty
    
    UI.create input output view

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

    UI.create input output view

let run ui events =
    ui.view.Subscribe (fun view -> printf "UI event: %A\n" view) |> ignore
    events |> List.iter (fun e ->
        ui.input.OnNext (e))            

run (clear (Solid 1) |> padded 5.0) [ WindowEvent { x = 200.0; y = 400.0 } ]
run (clear (Solid 1) |> margined 5.0) [ WindowEvent { x = 200.0; y = 400.0 } ]
run (clear (Solid 1) |> overlay (clear (Solid 2))) [ WindowEvent { x = 200.0; y = 400.0 } ]

run (clear (Solid 1) |> overlay (clear (Solid 2))) 
  [ WindowEvent { x = 200.0; y = 400.0 } 
    MouseEvent <| Some
      { location = 
          { x = 12.0
            y = 21.0 }
        leftButton = false
        rightButton = false }
  ]

let mouseString m =
    match m with
    | Some { location = { x = x; y = y } } -> sprintf "(%f, %f)" x y
    | None -> "no mouse"

run (clear (Solid 1) |> overlay (drawMouse (mouseString >> label >> Some))) 
  [ WindowEvent { x = 200.0; y = 400.0 } 
    MouseEvent <| Some
      { location = 
          { x = 12.0
            y = 21.0 }
        leftButton = false
        rightButton = false }
  ]

(* TODO:
    - Virtualization
    - Text size measuring
    - Feedback loops (connect output to input)
    - User (library user, not program user) input/output types
    - Actual rendering with Direct2D, etc
    - Integration with WPF and/or Windows Forms
    - Timing/animation
*)

module Windows =
    let rectangle r = 
        System.Drawing.RectangleF(
            System.Drawing.PointF(
                float32 r.topLeft.x, 
                float32 r.topLeft.y), 
            System.Drawing.SizeF(
                float32 r.size.x, 
                float32 r.size.y))

    let color (c: Color) = System.Drawing.Color.FromArgb(c)

    let brush b =
        match b with
        | Solid c -> new System.Drawing.SolidBrush(color c)

    let matrix m =
        new System.Drawing.Drawing2D.Matrix(
            float32 m.M11, 
            float32 m.M12, 
            float32 m.M21, 
            float32 m.M22, 
            float32 m.M31, 
            float32 m.M32)

    let rec render (graphics: System.Drawing.Graphics) view =
        match view with
        | DrawEmpty -> ()
        | DrawClipped (clip, drawing) ->
            let old = graphics.Clip

            use region = 
                new System.Drawing.Region(rectangle clip)

            graphics.Clip <- region
            render graphics drawing
            graphics.Clip <- old

        | DrawFill (r, b) ->
            use br = brush b
            graphics.FillRectangle(br, rectangle r)

        | DrawGroup drawings -> List.iter (render graphics) drawings

        | DrawRectangle (r, s) ->
            use b = brush s.brush
            use pen = new System.Drawing.Pen(b, float32 s.thickness)
            graphics.DrawRectangle(
                pen, 
                float32 r.topLeft.x, 
                float32 r.topLeft.y, 
                float32 r.size.x, 
                float32 r.size.y)

        | DrawText s -> 
            use b = new System.Drawing.SolidBrush(System.Drawing.Color.Black)
            use f = new System.Drawing.Font(System.Drawing.FontFamily.GenericMonospace, 10.0f)
            graphics.DrawString(s, f, b, 0.0f, 0.0f)

        | DrawTransformed (transform, drawing) ->
            let old = graphics.Transform
            use m = matrix transform
            graphics.Transform <- m
            render graphics drawing
            graphics.Transform <- old

let application = ref null

let wh = new System.Threading.ManualResetEvent(false)

let runApp () =
    try
        let app = System.Windows.Application()
        application := app
        wh.Set () |> ignore
        app.ShutdownMode <- System.Windows.ShutdownMode.OnExplicitShutdown
        printf "Running app\n"
        app.Run() |> ignore
        printf "App ended\n"
    with
    | _ as e -> printf "%O" e

let thread = System.Threading.Thread (runApp)
thread.IsBackground <- true
thread.SetApartmentState System.Threading.ApartmentState.STA
thread.Start()
wh.WaitOne() |> ignore

let post f =
    application.Value.Dispatcher.Invoke(System.Action< >(f))

post (fun () -> 
    printf "Creating window\n"
    (new System.Windows.Window()).Show())
