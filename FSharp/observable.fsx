
#r "../packages/System.Reactive.Core.3.0.0/lib/net45/System.Reactive.Core.dll"
#r "../packages/System.Reactive.Interfaces.3.0.0/lib/net45/System.Reactive.Interfaces.dll"
#r "../packages/System.Reactive.Linq.3.0.0/lib/net45/System.Reactive.Linq.dll"
#r "../packages/System.Reactive.PlatformServices.3.0.0/lib/net45/System.Reactive.PlatformServices.dll"
#r "../packages/System.Reactive.Windows.Threading.3.0.0/lib/net45/System.Reactive.Windows.Threading.dll"
#r "../packages/FSharp.Control.Reactive.3.5.0/lib/net45/FSharp.Control.Reactive.dll"

#r "../packages/SharpDX.Desktop.3.0.2/lib/net45/SharpDX.Desktop.dll"
#r "../packages/SharpDX.Direct2D1.3.0.2/lib/net45/SharpDX.Direct2D1.dll"
#r "../packages/SharpDX.Direct3D11.3.0.2/lib/net45/SharpDX.Direct3D11.dll"
#r "../packages/SharpDX.3.0.2/lib/net45/SharpDX.dll"
#r "../packages/SharpDX.DXGI.3.0.2/lib/net45/SharpDX.DXGI.dll"
#r "../packages/SharpDX.Mathematics.3.0.2/lib/net45/SharpDX.Mathematics.dll"

open FSharp.Control.Reactive

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
    | DrawClipped of Matrix3x2 * Drawing
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

open System.Collections.Generic
(*
open SharpDX.Direct2D1
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

type Resource =
    | Brush of Brush

type ResourceCache() =
    let mutable solidColorBrush = None
    
    let linearGradientBrushes = Dictionary<float32 * Matrix3x2, SharpDX.Direct2D1.LinearGradientBrush>()

    let gradientStopCollections = Dictionary<GradientStop list, SharpDX.Direct2D1.GradientStopCollection>()

    let strokeStyles = Dictionary<StrokeStyle, SharpDX.Direct2D1.StrokeStyle>()

    member this.getBrush target resource =
        match resource with
        | Solid color ->
            solidColorBrush <- 
                match solidColorBrush with 
                | Some s -> Some s 
                | None -> Some (new SolidColorBrush(target, Color.op_Implicit(color)))
            solidColorBrush.Value.Color <- Color.op_Implicit(color)
            solidColorBrush.Value :> SharpDX.Direct2D1.Brush

        | Linear linear ->
            let gradientStops =
                match gradientStopCollections.TryGetValue(linear.stops) with
                | (true, value) -> value
                | (false, _) ->
                    let stops =
                        new GradientStopCollection(
                                target, List.toArray linear.stops)
                    gradientStopCollections.Add(linear.stops, stops)
                    stops
        
            let brush =
                match linearGradientBrushes.TryGetValue((linear.opacity, linear.transform)) with
                | (true, value) -> value
                | (false, _) -> 
                    let brush =
                        new LinearGradientBrush(
                            target,
                            LinearGradientBrushProperties(
                                StartPoint = RawVector2(linear.start.x, linear.start.y),
                                EndPoint = RawVector2(linear.stop.x, linear.stop.y)),
                            System.Nullable(
                                BrushProperties(
                                    Opacity = linear.opacity,
                                    Transform = Matrix3x2.op_Implicit(linear.transform))),
                            gradientStops)
                    linearGradientBrushes.Add((linear.opacity, linear.transform), brush)
                    brush

            brush.StartPoint <- RawVector2(linear.start.x, linear.start.y)
            brush.EndPoint <- RawVector2(linear.stop.x, linear.stop.y)
            brush :> SharpDX.Direct2D1.Brush

        member this.getStrokeStyle (factory: Factory) resource =
            match strokeStyles.TryGetValue(resource) with
            | (true, s) -> s
            | (false, _) ->
                let s = new SharpDX.Direct2D1.StrokeStyle(factory, resource.properties, List.toArray resource.dashes)
                strokeStyles.Add(resource, s)
                s

        member this.ReleaseTargetResources() =
            solidColorBrush <-
                match solidColorBrush with
                | Some b -> b.Dispose(); None
                | None -> None

            for linear in linearGradientBrushes.Values do linear.Dispose()
            linearGradientBrushes.Clear()

            for grad in gradientStopCollections.Values do grad.Dispose()
            gradientStopCollections.Clear()

        member this.ReleaseFactoryResources() =
            for s in strokeStyles.Values do s.Dispose()
            strokeStyles.Clear()

let rec renderCommand (target: RenderTarget) (cache: ResourceCache) command =
    match command with
    | RectangleFill r -> 
        target.FillRectangle(
            RawRectangleF(
                r.geometry.topLeft.x,
                r.geometry.topLeft.y,
                r.geometry.bottomRight.x,
                r.geometry.bottomRight.y),
            cache.getBrush target r.brush)

    | RectangleStroke r -> 
        target.DrawRectangle(
            RawRectangleF(
                r.geometry.topLeft.x,
                r.geometry.topLeft.y,
                r.geometry.bottomRight.x,
                r.geometry.bottomRight.y),
            cache.getBrush target r.stroke.brush,
            r.stroke.width,
            match r.stroke.style with
            | Some s -> cache.getStrokeStyle target.Factory s
            | None -> null)

    | Clipped (rect, d) ->
        target.PushAxisAlignedClip(
            RawRectangleF(
                rect.topLeft.x, rect.topLeft.y, 
                rect.bottomRight.x, rect.bottomRight.y),
            AntialiasMode.Aliased)

        renderCommand target cache d

        target.PopAxisAlignedClip()

    | Transformed (matrix, d) ->
        let originalTransform = target.Transform

        target.Transform <- 
            Matrix3x2.op_Implicit(
                Matrix3x2.Multiply(
                    Matrix3x2.op_Implicit(originalTransform), 
                    matrix))

        renderCommand target cache d

        target.Transform <- originalTransform

    | Drawings cmds ->
        cmds |> List.iter (renderCommand target cache)

let render target cache drawing = renderCommand target cache drawing.drawing
*)