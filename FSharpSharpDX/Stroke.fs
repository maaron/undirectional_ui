module Stroke

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui
open Brush

module StrokeStyle =

    type Event =
      { properties: StrokeStyleProperties
        dashes: float32 list
      }

    type Model =
      { properties: Event
        resource: StrokeStyle option
      }

    let init = 
      { properties = 
          { properties = new StrokeStyleProperties()
            dashes = [] }
        resource = None
      }

    let release model =
        model.resource |> Option.map (fun r -> r.Dispose()) |> ignore
        { model with resource = None }

    let create (model: Model) (target: RenderTarget) =
        { release model with resource = Some (new StrokeStyle(target.Factory, model.properties.properties, List.toArray model.properties.dashes)) }

    let update event model target =
        create { model with properties = event } target

type Model = 
  { brush: Brush.Model
    width: float32
    style: StrokeStyle.Model option
  }

type Event =
  | Brush of Brush.Event
  | Width of float32
  | Style of StrokeStyle.Event option

let init =
  { brush = Brush.init
    width = 0.0f
    style = None
  }

let release model =
    { model with brush = release model.brush }

let create model target =
    { model with brush = Brush.create model.brush target }

let update event model target =
    match event with
    | Brush b -> { model with brush = Brush.update b model.brush target }
    | Width w -> { model with width = w }
    | Style s -> 
        let updated =
          match (model.style, s) with
          | (None, None) -> None
          | (Some s, None) -> Some (StrokeStyle.release s)
          | (None, Some e) -> Some (StrokeStyle.update e StrokeStyle.init target)
          | (Some s, Some e) -> Some (StrokeStyle.update e s target)
        { model with style = updated }