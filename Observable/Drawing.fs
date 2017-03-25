module Drawing

open Geometry

type Color = int

type Brush =
    | Solid of Color

type Stroke =
  { brush: Brush
    thickness: float }

type Drawing = 
    | DrawEmpty
    | DrawFill of Rectangle * Brush
    | DrawRectangle of Rectangle * Stroke
    | DrawText of string
    | DrawTransformed of Matrix3x2 * Drawing
    | DrawClipped of Matrix3x2 * Drawing
    | DrawGroup of Drawing list
