module Overlayed

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui

let maxSize (s1: Size2F) (s2: Size2F) =
    Size2F(max s1.Width s2.Width, max s1.Height s2.Height)

type Event<'b, 't> =
  | Bottom of 'b
  | Top of 't

type Model<'m1, 'm2> =
    {
    bounds: Size2F
    bottom: 'm1
    top: 'm2
    }

let overlayed (top: Ui<'e2, 'm2>) (bottom: Ui<'e1, 'm1>): Ui<Event<'e1, 'e2>, Model<'m1, 'm2>> =
  { init = 
        let (m1, cmd1) = bottom.init
        let (m2, cmd2) = top.init
        let model =
            {
            bounds = Size2F.Zero
            bottom = m1
            top = m2
            }
        (model, Cmd.batch [Cmd.map Bottom cmd1; Cmd.map Top cmd2])

    bounds = fun model -> bottom.bounds model.bottom

    view = 
        fun model target ->
            bottom.view model.bottom target
            top.view model.top target

    update = 
        fun event model ->
            
            let (bottomModel2, bottomCmd) = 
                match event with
                | Event (Bottom bottomEvent) -> bottom.update (Event bottomEvent) model.bottom
                | Event (Top topEvent) -> (model.bottom, Cmd.none)
                | Input i -> bottom.update (Input i) model.bottom
                | Resource r -> bottom.update (Resource r) model.bottom
                | Bounds b -> bottom.update (Bounds b) model.bottom

            let bounds2 = bottom.bounds bottomModel2
            let (topModelSized, topModelCmd) =
                if model.bounds = bounds2 then
                    (model.top, Cmd.none)
                else
                    top.update (Bounds bounds2) model.top

            let (topModel2, topModelCmd2) =
                match event with
                | Event (Top topEvent) -> top.update (Event topEvent) topModelSized
                | Event (Bottom bottomEvent) -> (topModelSized, Cmd.none)
                | Input i -> top.update (Input i) topModelSized
                // Bounds for the overlayed UI are determined by the UI underneath
                | Bounds b -> (topModelSized, Cmd.none)
                | Resource r -> top.update (Resource r) topModelSized
            
            ({ bounds = bounds2; bottom = bottomModel2; top = topModel2 }, Cmd.batch [Cmd.map Bottom bottomCmd; Cmd.map Top topModelCmd; Cmd.map Top topModelCmd2])
  }