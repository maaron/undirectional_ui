module Drawing

open SharpDX
open SharpDX.Direct2D1
open Geometry

type StrokeStyle = {
    properties: SharpDX.Direct2D1.StrokeStyleProperties
    dashes: float32 list
}

type LinearGradientModel = 
  { start: Point
    stop: Point
    opacity: float32
    transform: Matrix3x2
    stops: GradientStop list
  }

type Brush = 
  | Solid of Color
  | Linear of LinearGradientModel

type Stroke = {
    brush: Brush
    width: float32
    style: StrokeStyle option
}

type RectangleFill = {
    geometry: Geometry.Rectangle
    brush: Brush
}

type RectangleStroke = {
    geometry: Geometry.Rectangle
    stroke: Stroke
}

type Drawing =
    | RectangleFill of RectangleFill
    | RectangleStroke of RectangleStroke
    | Clipped of Rectangle * Drawing
    | Transformed of Matrix3x2 * Drawing
    | Drawings of Drawing list

let transformed matrix cmd = Transformed (matrix, cmd)

let clipped rect cmd = Clipped (rect, cmd)

