module Overlayed

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui

type Event<'b, 't> =
  | Bottom of 'b
  | Top of 't

let overlayed (bottom: Interface<'e1, 'm1, 'c>) (top: Interface<'e2, 'm2, 'c>): Interface<Event<'e1, 'e2>, ContentModel<'m1> * ContentModel<'m2>, 'c> =
  { init = 
        let (m1, cmd1) = bottom.init
        let (m2, cmd2) = top.init
        let model =
          { bounds = Size2F.Zero 
            content = (m1, m2)
          }
        (model, Cmd.batch [Cmd.map Bottom cmd1; Cmd.map Top cmd2])

    view = 
        fun m rt ->
            let (m1, m2) = m.content
            bottom.view m1 rt
            top.view m2 rt

    update = 
        fun e m ->
            match e with
            | Event (Bottom e1) -> 
                let (m1, c1) = bottom.update (Event e1) (fst m.content)
                ({ m with content = (m1, snd m.content) }, Cmd.map Bottom c1)

            | Event (Top e2) -> 
                let (msub, csub) = top.update (Event e2) (snd m.content)
                ({ m with content = (fst m.content, msub) }, Cmd.map Top csub)

            | Input i ->
                let (m1, c1) = bottom.update (Input i) (fst m.content)
                let (m2, c2) = top.update (Input i) (snd m.content)
                ({ m with content = (m1, m2)}, Cmd.batch [Cmd.map Bottom c1; Cmd.map Top c2])

            | Content (Bounds b) ->
                let (m1, c1) = bottom.update (Content (Bounds b)) (fst m.content)
                let (m2, c2) = top.update (Content (Bounds b)) (snd m.content)
                ({ bounds = b; content = (m1, m2)}, Cmd.batch [Cmd.map Bottom c1; Cmd.map Top c2])

            | Content c ->
                let (m1, c1) = bottom.update (Content c) (fst m.content)
                let (m2, c2) = top.update (Content c) (snd m.content)
                ({ m with content = (m1, m2)}, Cmd.batch [Cmd.map Bottom c1; Cmd.map Top c2])
  }