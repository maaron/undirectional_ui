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
open Mapped
open Augmented

type Event<'a> =
  | Border of Stroke.Event
  | BorderContent of 'a

#if false
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
#endif

type FrameEvent = float32

let frame =
    {
    init = 
        let (rectmodel, rectcmd) = rectangleStrokeDefault.init
        let model =
            {
            bounds = Size2F.Zero
            content = (rectmodel.bounds, rectmodel)
            }
        (model, rectcmd)

    view = fun model -> rectangleStrokeDefault.view (snd (model.content))

    update =
        fun event model ->
            let (width, rect) = model.content
            match event with
            | Content (Bounds b) -> sendEvents [] rectangleStrokeDefault.update
    }

let bordered props ui =
    let width = 
        defaultArg (props |> List.tryPick (fun p -> match p with | Width w -> Some w | _ -> None)) 0.0f
    
    let border = 
        initialize rectangleStrokeDefault (props |> List.map (fun p -> Stroke p))
     |> onsize 
          ( fun s -> 
                let halfWidth = width / 2.0f
                sendEvents 
                  [ Size (Size2F(s.Width - halfWidth, s.Height - halfWidth))
                    TopLeft (Vector2(halfWidth, halfWidth))
                  ]
          )

    ui
 |> padded width
 |> overlayed border

 |> mapMany
        (fun e -> 
            match e with 
            | Border (Width w) -> [Top (Stroke (Width w)); Bottom (Padding w)]
            | Border b -> [Top (Stroke b)]
            | BorderContent c -> [Top c])
        (fun e ->
            match e with
            | Bottom (Padded b) -> Border b
            | Bottom (Padding p) -> Border (Width p)
            | Top c -> BorderContent c)
