module RectangleStroke

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui
open Brush
open Stroke
open Resource

type Event = 
  | Size of Size2F
  | TopLeft of Vector2
  | Stroke of Stroke.Event

type Model = 
  { topLeft: Vector2
    bounds: Size2F
    stroke: Stroke.Model
  }

let rectangleStrokeDefault: Ui.Ui<Event, Model> =
  { init =
        let model = 
            {
            topLeft = Vector2(0.0f, 0.0f)
            bounds = Size2F.Zero
            stroke = Stroke.init 
            }
        (model, Cmd.none)

    bounds = fun size model -> model.bounds
    
    view = 
        fun m rt -> 
            m.stroke.brush |> Resource.map
              ( fun resource ->
                    let brush = brush resource
                    
                    let style = 
                        match m.stroke.style with
                        | Some (props, s) -> 
                            match s with
                            | Created (target, resource) -> resource
                            | Released -> null
                        | _ -> null

                    rt.DrawRectangle(
                        RawRectangleF(
                            m.topLeft.X, m.topLeft.Y, 
                            m.bounds.Width, m.bounds.Height), 
                        brush, 
                        m.stroke.width,
                        style)
              ) |> ignore

    update =
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)

            | Event (TopLeft p) -> ({ m with topLeft = p }, Cmd.none)

            | Event (Stroke stroke) ->
                ({ m with stroke = Stroke.update stroke m.stroke }, Cmd.none)

            | Resource (Create rt) -> 
                ({ m with stroke = Stroke.create m.stroke rt }, Cmd.none)
            
            | Resource (Release) ->
                ({ m with stroke = Stroke.release m.stroke }, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangleStroke = initialize rectangleStrokeDefault