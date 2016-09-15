
module Geometry

open SharpDX

type Point = 
    { x: float32
      y: float32
    }
with
    static member zero = { x = 0.0f; y = 0.0f }
    static member minCoords p1 p2 = { x = min p1.x p2.x; y = min p1.y p2.y }
    static member maxCoords p1 p2 = { x = max p1.x p2.x; y = max p1.y p2.y }
    static member transform matrix point = 
        let vector = Matrix3x2.TransformPoint(matrix, Vector2(point.x, point.y))
        { x = vector.X; y = vector.Y }

type Rectangle = 
    { topLeft: Point
      bottomRight: Point
    }
with
    member this.bottomLeft = {x = this.topLeft.x; y = this.bottomRight.y }
    
    member this.topRight = {x = this.bottomRight.x; y = this.topLeft.y }
    
    static member fromPoints p1 p2 =
        { 
            topLeft = Point.minCoords p1 p2
            bottomRight = Point.maxCoords p1 p2
        }

    static member union r1 r2 = 
        {
            topLeft = Point.minCoords r1.topLeft r2.topLeft
            bottomRight = Point.maxCoords r1.bottomRight r2.bottomRight
        }

    static member transformBounds matrix rectangle =
        let (p1, p2, p3, p4) = (
            Point.transform matrix rectangle.topLeft,
            Point.transform matrix rectangle.topRight,
            Point.transform matrix rectangle.bottomLeft,
            Point.transform matrix rectangle.bottomRight)
        {
            topLeft = Point.minCoords (Point.minCoords p1 p2) (Point.minCoords p3 p4)
            bottomRight = Point.maxCoords (Point.maxCoords p1 p2) (Point.maxCoords p3 p4)
        }

    static member infinity =
        {
            topLeft = { x = -infinityf; y = -infinityf }
            bottomRight = { x = infinityf; y = infinityf }
        }

    static member zero =
        {
            topLeft = Point.zero
            bottomRight = Point.zero
        }

    static member containsPoint point rectangle =
        point.x >= rectangle.topLeft.x && 
        point.x < rectangle.bottomRight.x &&
        point.y >= rectangle.topLeft.y &&
        point.y < rectangle.bottomRight.y

let maxSize (s1: Size2F) (s2: Size2F) =
    Size2F(max s1.Width s2.Width, max s1.Height s2.Height)

let minSize (s1: Size2F) (s2: Size2F) =
    Size2F(min s1.Width s2.Width, min s1.Height s2.Height)

module M32 =
    open SharpDX.Mathematics.Interop

    let translate (matrix32: RawMatrix3x2) x y =
        RawMatrix3x2(
            m11 = matrix32.M11,
            m12 = matrix32.M12,
            m21 = matrix32.M21,
            m22 = matrix32.M22,
            m31 = matrix32.M31 + x,
            m32 = matrix32.M32 + y)
