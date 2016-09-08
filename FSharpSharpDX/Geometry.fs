
namespace Geometry

type Size = { width: float32; height: float32 }

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

module Rect =
    open SharpDX.Mathematics.Interop
    
    let translate (rect: RawRectangleF) x y =
        RawRectangleF(
            )