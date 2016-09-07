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
  | Stroke of Stroke.Event

let rectangleStrokeDefault: Ui.Interface<Event, Stroke.Model> =
  { init =
        let model = 
          { bounds = Size2F.Zero
            content = Stroke.init }
        let cmd = Ui.Cmd.none
        (model, cmd)

    view = 
        fun m rt -> 
            m.content.brush |> Resource.map
              ( fun resource ->
                    let brush = brush resource
                    
                    let style = 
                        match m.content.style with
                        | Some (props, s) -> 
                            match s with
                            | Created (target, resource) -> resource
                            | Released -> null
                        | _ -> null

                    rt.DrawRectangle(
                        RawRectangleF(
                            0.0f, 0.0f, 
                            m.bounds.Width, 
                            m.bounds.Height), 
                        brush, 
                        m.content.width,
                        style)
              ) |> ignore

    update =
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)

            | Event (Stroke stroke) ->
                ({ m with content = Stroke.update stroke m.content }, Cmd.none)

            | Content (Resource (Create rt)) -> 
                ({ m with content = Stroke.create m.content rt }, Cmd.none)
            
            | Content (Resource (Release)) ->
                ({ m with content = Stroke.release m.content }, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangleStroke = initialize rectangleStrokeDefault