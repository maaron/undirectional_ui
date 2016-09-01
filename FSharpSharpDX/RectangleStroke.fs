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

type Event =
    | Size of Size2F
    | Stroke of Brush.Model
    | StrokeWidth of float32

type RectangleStrokeResource = 
  { target: RenderTarget
    stroke: Brush.Resource
  }
  with 
    member x.Dispose() = 
        x.stroke.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

type RectangleStrokeModel = {
    stroke: Brush.Model
    strokeWidth: float32
    strokeStyle: StrokeStyle option
    fill: Brush.Model option
} with
    member x.create rt = 
      { target = rt
        stroke = x.stroke.create rt
      }

    member x.update (resource: RectangleStrokeResource) =
      resource.Dispose()
      x.create resource.target

let rectangleStrokeDefault: Ui.Interface<Event, ResourceModel<RectangleStrokeModel, RectangleStrokeResource>, unit> =
  { init =
        let model = 
          { bounds = Size2F.Zero
            content = 
              { properties =
                  { stroke = Solid Color.Transparent
                    strokeWidth = 0.0f 
                    strokeStyle = None
                    fill = None }
                resource = None } }
        let cmd = Ui.Cmd.none
        (model, cmd)

    view = 
        fun m rt -> 
            m.content.resource |> Option.map
              ( fun resource ->
                    rt.DrawRectangle(
                        RawRectangleF(
                            0.0f, 0.0f, 
                            m.bounds.Width, 
                            m.bounds.Height), 
                        resource.stroke.brush, 
                        m.content.properties.strokeWidth,
                        null)
              ) |> ignore

    update =
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)

            | Event (Stroke stroke) ->
                let properties = { m.content.properties with stroke = stroke }
                let resource = m.content.resource |> Option.map properties.update
                ({ m with content = { m.content with properties = properties; resource = resource } }, Cmd.none)

            | Event (StrokeWidth width) ->
                ({ m with content = { m.content with properties = { m.content.properties with strokeWidth = width }}}, Cmd.none)
            
            | Content (Resource (Create rt)) -> 
                let resource = Some (m.content.properties.create rt)
                ({ m with content = { m.content with resource = resource } }, Cmd.none)
            
            | Content (Resource (Release)) ->
                m.content.resource |> Option.map (fun f -> f.Dispose()) |> ignore
                ({ m with content = { m.content with resource = None }}, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangleStroke = initialize rectangleStrokeDefault