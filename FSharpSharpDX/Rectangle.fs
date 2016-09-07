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
    | Fill of Brush.Event

let rectangleDefault: Interface<Event, Brush.Model> =
  { init =
        let model = 
          { bounds = Size2F.Zero
            content = Brush.init }
        let cmd = Cmd.none
        (model, cmd)

    view = 
        fun m rt -> 
            m.content |> Resource.map
              ( fun resource ->
                    rt.FillRectangle(
                        RawRectangleF(
                            0.0f, 0.0f, 
                            m.bounds.Width, 
                            m.bounds.Height), 
                        brush resource)
              ) |> ignore

    update =
        //let updateProp prop m = { m with content = { m.content with properties = prop } }
        //let updateResource res m = { m with content = { m.content with resource = res } }
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)

            | Event (Fill fill) ->
                ({ m with content = Brush.update fill m.content }, Cmd.none)
            
            | Content (Resource (Create rt)) -> 
                ({ m with content = Brush.create m.content rt }, Cmd.none)
            
            | Content (Resource (Release)) ->
                ({ m with content = Brush.release m.content }, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangle = initialize rectangleDefault