
namespace Draw.Primitive

open SharpDX
open SharpDX.Direct2D1

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