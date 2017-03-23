module Rectangle

open SharpDX
open Ui
open Cmd
open Geometry
open Draw.Drawing
open Draw.Primitive

let rectangle (model: RectangleFill) =
    {
    init = ((model, ()), Cmd.none)

    view =
        fun (model, _) ->
          { size = model.geometry.bottomRight
            command = RectangleFill model }

    update =
        fun event model -> (model, Cmd.none)
    }

let geometry g ((m: RectangleFill), s) = ({ m with geometry = g }, s), Cmd.none

let brush b ((m: RectangleFill), s) = ({ m with brush = b }, s), Cmd.none