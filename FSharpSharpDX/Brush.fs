
module Brush

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop

open Ui
open Resource

type Resource =
  | Solid of SolidColorBrush
  | Linear of LinearGradientBrush * GradientStopCollection
  with
    member x.Dispose() =
        match x with
        | Solid brush -> brush.Dispose()
        | Linear (brush, grad) -> brush.Dispose(); grad.Dispose()

let brush model =
    match model with
    | Solid brush -> brush :> Brush
    | Linear (brush, grad) -> brush :> Brush

type LinearGradientModel = 
  { start: Vector2
    stop: Vector2
    opacity: float32
    transform: Matrix3x2
    stops: GradientStop list
  }

type Properties = 
  | Solid of Color
  | Linear of LinearGradientModel

type Event = Properties

type Model = TargetBound<Properties, Resource>

let init: Model = (Solid Color.Transparent, Released)

let release (model: Model) =
    model |> Resource.map (fun r -> r.Dispose())

let create model target =
    let (props, state) = model
    let resource = 
        match props with
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

    (fst (release model), Created (target, resource))

let update event model =
    let (props, state) = model
    match state with
    | Created (target, resource) -> create (event, state) target
    | Released -> (event, Released)