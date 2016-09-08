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

let overlayed (top: Interface<'e2, 'm2>) (bottom: Interface<'e1, 'm1>): Interface<Event<'e1, 'e2>, ContentModel<'m1> * ContentModel<'m2>> =
  { init = 
        let (m1, cmd1) = bottom.init
        let (m2, cmd2) = top.init
        let model =
          { bounds = Size2F(max m1.bounds.Width m2.bounds.Width, max m1.bounds.Height m2.bounds.Height)
            content = (m1, m2)
          }
        (model, Cmd.batch [Cmd.map Bottom cmd1; Cmd.map Top cmd2])

    view = 
        fun m rt ->
            let (m1, m2) = m.content
            bottom.view m1 rt
            top.view m2 rt

    update = 
        fun event model ->
            let { bounds = bounds; content = (bottomModel, topModel) } = model
            
            let (bottomModel2, bottomCmd) = 
                match event with
                | Event (Bottom bottomEvent) -> bottom.update (Event bottomEvent) bottomModel
                | Event (Top topEvent) -> (bottomModel, Cmd.none)
                | Input i -> bottom.update (Input i) bottomModel
                | Content c -> bottom.update (Content c) bottomModel

            let (topModelSized, topModelCmd) =
                if bottomModel.bounds = bottomModel2.bounds then
                    (topModel, Cmd.none)
                else
                    top.update (Content (Bounds bottomModel2.bounds)) topModel

            let (topModel2, topModelCmd2) =
                match event with
                | Event (Top topEvent) -> top.update (Event topEvent) topModelSized
                | Event (Bottom bottomEvent) -> (topModelSized, Cmd.none)
                | Input i -> top.update (Input i) topModelSized
                // Bounds for the overlayed UI are determined by the UI underneath
                | Content (Bounds b) -> (topModelSized, Cmd.none)
                | Content c -> top.update (Content c) topModelSized
            
            ({ bounds = bottomModel2.bounds; content = (bottomModel2, topModel2) }, Cmd.batch [Cmd.map Bottom bottomCmd; Cmd.map Top topModelCmd; Cmd.map Top topModelCmd2])
  }