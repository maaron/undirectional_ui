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
    stroke: Stroke.Model
  }

let rectangleStrokeDefault: Ui.Interface<Event, Model> =
  { init =
        let model = 
          { bounds = Size2F.Zero
            content = 
              { topLeft = Vector2(0.0f, 0.0f)
                stroke = Stroke.init 
              }
          }
        let cmd = Ui.Cmd.none
        (model, cmd)

    view = 
        fun m rt -> 
            m.content.stroke.brush |> Resource.map
              ( fun resource ->
                    let brush = brush resource
                    
                    let style = 
                        match m.content.stroke.style with
                        | Some (props, s) -> 
                            match s with
                            | Created (target, resource) -> resource
                            | Released -> null
                        | _ -> null

                    rt.DrawRectangle(
                        RawRectangleF(
                            m.content.topLeft.X, m.content.topLeft.Y, 
                            m.bounds.Width, m.bounds.Height), 
                        brush, 
                        m.content.stroke.width,
                        style)
              ) |> ignore

    update =
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)

            | Event (TopLeft p) -> ({ m with content = { m.content with topLeft = p } }, Cmd.none)

            | Event (Stroke stroke) ->
                ({ m with content = { m.content with stroke = Stroke.update stroke m.content.stroke } }, Cmd.none)

            | Content (Resource (Create rt)) -> 
                ({ m with content = { m.content with stroke = Stroke.create m.content.stroke rt } }, Cmd.none)
            
            | Content (Resource (Release)) ->
                ({ m with content = { m.content with stroke = Stroke.release m.content.stroke } }, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangleStroke = initialize rectangleStrokeDefault