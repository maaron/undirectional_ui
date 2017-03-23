module Rectangle

open SharpDX
open Ui
open Cmd
open Geometry
open Drawing
open View

type Event =
    | TopLeft of Point
    | Size of Point
    | Fill of Brush

let rectangleDefault =
    {
    init = 
        let model =
            {
            geometry = Rectangle.zero
            brush = Solid Color.Transparent
            }
        (model, Cmd.none)

    view =
        fun model ->
            {
            size = model.geometry.bottomRight
            drawing = RectangleFill model
            }

    update =
        fun event model ->
            match event with
            | Event (TopLeft p) -> ({ model with geometry = { model.geometry with topLeft = p } }, Cmd.none)
            | Event (Size s) -> ({ model with geometry = { model.geometry with bottomRight = { x = model.geometry.topLeft.x + s.x; y = model.geometry.topLeft.y + s.y } } }, Cmd.none)
            | Event (Fill b) -> ({ model with brush = b }, Cmd.none)
            | _ -> (model, Cmd.none)
    }

let rectangle events = initialize rectangleDefault events