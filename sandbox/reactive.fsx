
open System

type Point = { x: float; y: float }
type Size = { width: float; height: float }
type Rect = { topLeft: Point; size: Size }
type Color = class end
type Brush =
    | Solid of Color
type Stroke = Brush
type Matrix3x2 = class end

type Mouse =
  { pointer: Point
    leftButton: bool
    rightButton: bool }

type Drawing =
    | Fill of Rect * Brush
    | Rectangle of Rect * Stroke
    | Transformed of Matrix3x2 * Drawing
    | Clipped of Rect * Drawing

type Image =
  { size: Size
    drawing: Drawing }

type Signal<'a> = IObservable<'a>

type UI<'a> = Signal<'a * Drawing>

let mouse (ui: UI<'a>): UI<'a * Mouse> =
    let c1 = ui |> Observable.map Choice1Of2
    let c2 = System.Obs