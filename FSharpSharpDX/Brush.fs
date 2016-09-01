
module Brush

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop

type Resource =
    | Solid of SolidColorBrush
    | Linear of LinearGradientBrush * GradientStopCollection
with
    member x.brush = 
        match x with
        | Solid brush -> brush :> Brush
        | Linear (brush, grad) -> brush :> Brush

    member x.Dispose() =
            match x with
            | Solid brush -> brush.Dispose()
            | Linear (brush, grad) -> brush.Dispose(); grad.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

type LinearGradientModel = {
    start: Vector2
    stop: Vector2
    opacity: float32
    transform: Matrix3x2
    stops: GradientStop list
} with
    member x.create rt =
        let linearBrushProperties = 
            LinearGradientBrushProperties(
                StartPoint = Vector2.op_Implicit(x.start), 
                EndPoint = Vector2.op_Implicit(x.stop))

        let brushProperties = 
            Nullable<BrushProperties>(
                BrushProperties(
                    Transform = Matrix3x2.op_Implicit(x.transform),
                    Opacity = x.opacity))
        
        let gradient = new GradientStopCollection(rt, List.toArray x.stops)

        let brush = new LinearGradientBrush(rt, linearBrushProperties, brushProperties, gradient)

        Resource.Linear (brush, gradient)

type Model =
  | Solid of Color
  | Linear of LinearGradientModel
  with
    member x.create rt =
        match x with
        | Solid color -> Resource.Solid (new SolidColorBrush(rt, Color.op_Implicit(color)))
        | Linear linear -> linear.create rt

    member x.update rt (resource: Resource) =
        resource.Dispose()
        x.create rt

