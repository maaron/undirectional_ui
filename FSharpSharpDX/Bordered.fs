module Bordered

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows
open Brush

open Ui
open RectangleStroke
open Overlayed
open Arranged
open Stroke

type Event<'a> =
  | Border of Stroke.Event
  | Child of 'a

let bordered ui =
  { init = 
        let (uimodel, uicmd) = ui.init
        let (rectmodel, rectcmd) = rectangleStrokeDefault.init
        let model = 
          { bounds = uimodel.bounds; content = (rectmodel, uimodel) }
        (model, Cmd.batch [uicmd; rectcmd])

    view =
        fun model target ->
            let (rect, content) = model.content
            rectangleStrokeDefault.view rect target
            ui.view content target

    update =
        fun event model ->
            let (rect, uimodel) = model.content
            match event with
            | Event (Border stroke) -> (rectangleStrokeDefault.update (Stroke stroke) rect, uimodel)

            | Child uievent ->
                let (c2, uicmd) = ui.update uievent uimodel
                let (rect2, rectcmd) = 
                    if c2.bounds != uimodel.bounds then 
                        rectangleStrokeDefault.update (Size c2.bounds + rect.content.width / 2) rect
                    else (rect, Cmd.none)
                ({ bounds = Size2F(rect2.bounds.Width + rect2.content.width / 2, rect2.bounds.Height + rect2.content.width / 2); content = (rect2, c2) }, Cmd.batch [uicmd; rectcmd])
  }

let borderedAlmost props ui =
    let width = 
        defaultArg (props |> List.tryPick (fun p -> match p with | Width w -> Some w | _ -> None)) 0.0f
    
    let border = 
        initialize rectangleStrokeDefault (props |> List.map (fun p -> Stroke p))

    let content = 
        ui
     |> padded width

    overlayed border content
 |> onsize (fun s -> sendEvents [Bottom (Size s)])
