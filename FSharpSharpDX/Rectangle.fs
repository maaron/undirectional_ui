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

type Model =
    {
    topLeft: Vector2
    bounds: Size2F
    brush: Brush.Model
    }

let rectangleDefault: Ui<Event, Model> =
  { init =
        let model = 
          { topLeft = Vector2.Zero
            bounds = Size2F.Zero
            brush = Brush.init }
        (model, Cmd.none)

    bounds = fun m -> m.bounds

    view = 
        fun m rt -> 
            m.brush |> Resource.map
              ( fun resource ->
                    rt.FillRectangle(
                        RawRectangleF(
                            m.topLeft.X, m.topLeft.Y, 
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
                ({ m with brush = Brush.update fill m.brush }, Cmd.none)
            
            | Resource (Create rt) -> 
                ({ m with brush = Brush.create m.brush rt }, Cmd.none)
            
            | Resource (Release) ->
                ({ m with brush = Brush.release m.brush }, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let rectangle = initialize rectangleDefault