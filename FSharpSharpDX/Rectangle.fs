module Rectangle

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
    | Fill of Brush.Model

type RectangleResource =
  { target: RenderTarget
    fill: Brush.Resource
  }
    member x.Dispose() = 
        x.fill.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

type RectangleModel = {
    fill: Brush.Model
} with
    member x.create rt = 
      { target = rt
        fill = x.fill.create rt
      }

    member x.update (resource: RectangleResource) =
      resource.Dispose()
      x.create resource.target

let rectangleDefault: Interface<Event, ResourceModel<RectangleModel, RectangleResource>, unit> =
  { init =
        let model = 
          { bounds = Size2F.Zero
            content = 
              { properties =
                  { fill = Solid Color.Transparent }
                resource = None } }
        let cmd = Cmd.none
        (model, cmd)

    view = 
        fun m rt -> 
            m.content.resource |> Option.map
              ( fun resource ->
                    rt.FillRectangle(
                        RawRectangleF(
                            0.0f, 0.0f, 
                            m.bounds.Width, 
                            m.bounds.Height), 
                        resource.fill.brush)
              ) |> ignore

    update =
        //let updateProp prop m = { m with content = { m.content with properties = prop } }
        //let updateResource res m = { m with content = { m.content with resource = res } }
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)

            | Event (Fill fill) ->
                let properties = { m.content.properties with fill = fill }
                let resource = m.content.resource |> Option.map properties.update
                ({ m with content = { m.content with properties = properties; resource = resource } }, Cmd.none)
            
            | Content (Resource (Create rt)) -> 
                let resource = Some (m.content.properties.create rt)
                ({ m with content = { m.content with resource = resource } }, Cmd.none)
            
            | Content (Resource (Release)) ->
                m.content.resource |> Option.map (fun f -> f.Dispose()) |> ignore
                ({ m with content = { m.content with resource = None }}, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangle = initialize rectangleDefault