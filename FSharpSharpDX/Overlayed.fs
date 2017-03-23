module Overlayed

open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui
open Cmd
open Geometry
open Drawing
open View

type Event<'b, 't> =
  | Bottom of 'b
  | Top of 't

type Model<'m1, 'm2> =
    {
    available: Point
    bottom: 'm1
    top: 'm2
    bottomDrawing: View
    }

let overlayed (top: Ui<'e2, 'm2>) (bottom: Ui<'e1, 'm1>): Ui<Event<'e1, 'e2>, Model<'m1, 'm2>> =
    {
    init = 
        let (m1, cmd1) = bottom.init
        let (m2, cmd2) = top.init
        let model =
            {
            available = Point.zero
            bottom = m1
            top = m2
            bottomDrawing = bottom.view m1
            }
        (model, Cmd.batch [Cmd.map Bottom cmd1; Cmd.map Top cmd2])

    view = 
        fun model ->
            {
                size = model.bottomDrawing.size
                drawing = Drawings
                  [ model.bottomDrawing.drawing
                    (top.view model.top).drawing ]
            }

    update = 
        fun event model ->
            let newAvailable = 
                match event with
                | Bounds b -> b
                | _ -> model.available
            
            let (bottomModel2, bottomCmd) = 
                match event with
                | Event (Bottom bottomEvent) -> bottom.update (Event bottomEvent) model.bottom
                | Event (Top topEvent) -> (model.bottom, Cmd.none)
                | Input i -> bottom.update (Input i) model.bottom
                | Bounds b -> bottom.update (Bounds b) model.bottom

            let newBottom = bottom.view bottomModel2
            let (topModelSized, topModelCmd) =
                if model.bottomDrawing.size = newBottom.size && model.available = newAvailable then
                    (model.top, Cmd.none)
                else
                    top.update (Bounds newBottom.size) model.top

            let (topModel2, topModelCmd2) =
                match event with
                | Event (Top topEvent) -> top.update (Event topEvent) topModelSized
                | Event (Bottom bottomEvent) -> (topModelSized, Cmd.none)
                | Input i -> top.update (Input i) topModelSized
                // Bounds for the overlayed UI are determined by the UI underneath
                | Bounds b -> (topModelSized, Cmd.none)

            (
                {
                    available = newAvailable
                    bottom = bottomModel2
                    top = topModel2
                    bottomDrawing = newBottom
                }, 
                Cmd.batch 
                    [
                    Cmd.map Bottom bottomCmd
                    Cmd.map Top topModelCmd
                    Cmd.map Top topModelCmd2
                    ]
            )
    }