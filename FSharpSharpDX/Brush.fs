
module Brush

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop

open Ui

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

type LinearGradientModel = 
  { start: Vector2
    stop: Vector2
    opacity: float32
    transform: Matrix3x2
    stops: GradientStop list
  }

type Event =
  | Solid of Color
  | Linear of LinearGradientModel

type Model =
  { properties: Event
    resource: Resource option
  }

let init = 
  { properties = Solid Color.Transparent
    resource = None
  }

let release model =
    model.resource |> Option.map (fun r -> r.Dispose()) |> ignore
    { model with resource = None }

let create model target =
    let resource = 
        match model.properties with
        | Solid color -> Resource.Solid (new SolidColorBrush(target, Color.op_Implicit(color)))
    
        | Linear linear ->
            let linearBrushProperties = 
                LinearGradientBrushProperties(
                    StartPoint = Vector2.op_Implicit(linear.start), 
                    EndPoint = Vector2.op_Implicit(linear.stop))

            let brushProperties = 
                Nullable<BrushProperties>(
                    BrushProperties(
                        Transform = Matrix3x2.op_Implicit(linear.transform),
                        Opacity = linear.opacity))
        
            let gradient = new GradientStopCollection(target, List.toArray linear.stops)

            let brush = new LinearGradientBrush(target, linearBrushProperties, brushProperties, gradient)

            Resource.Linear (brush, gradient)

    { release model with resource = Some resource }

let update event model target =
    create { model with properties = event } target