module Geometry

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
