module Bordered

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Draw.Primitive
open Draw.Drawing
open Ui
open Cmd
open Overlayed
open Arranged
open Padded
open Mapped
open Augmented
open Arranged
open Geometry

type Event<'a> =
  | Width of float32
  | Brush of Brush
  | Style of StrokeStyle option
  | Content of 'a

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

type Model<'e, 'm> = {
    stroke: Stroke
    subUi: Ui<'e, 'm>
    subModel: 'm
    subDrawing: Drawing
}

let borderedDefault ui =
    {
    init = 
        let arrangedUi = arranged identityArranger ui
        let (sub, cmd) = arrangedUi.init
        let model = 
            {
                stroke =
                    {
                    brush = Solid Color.Transparent
                    width = 0.0f
                    style = None
                    }
                subUi = arrangedUi
                subModel = sub
                subDrawing = arrangedUi.view sub
            }
        (model, Cmd.map Content cmd)

    view =
        fun { stroke = stroke; subUi = subUi; subDrawing = subDrawing } ->
            let width = stroke.width
            let doubleWidth = width + 2.0f
            let halfWidth = width / 2.0f
            { 
                size = subDrawing.size
                clip = None
                transform = Matrix3x2.Identity
                commands = 
                    [
                    Drawing subDrawing
                    RectangleStroke 
                        {
                        geometry = 
                            {
                                topLeft = { x = halfWidth; y = halfWidth }
                                bottomRight = { x = subDrawing.size.x - halfWidth; y = subDrawing.size.y - halfWidth }
                            }
                        stroke = stroke
                        }
                    ]
            }

    update =
        fun event model ->
            let updateSub event model =
                let (subModel2, cmd) = model.subUi.update event model.subModel
                let subDrawing2 = model.subUi.view subModel2
                ({ model with subModel = subModel2; subDrawing = subDrawing2 }, Cmd.map Content cmd)

            match event with
            | Event (Width w) -> ({ model with stroke = { model.stroke with width = w }; subUi = padded w ui }, Cmd.none)
            | Event (Brush b) -> ({ model with stroke = { model.stroke with brush = b } }, Cmd.none)
            | Event (Style s) -> ({ model with stroke = { model.stroke with style = s } }, Cmd.none)
            | Event (Content c) -> updateSub (Event c) model
            | Input i -> updateSub (Input i) model
            | Bounds b -> updateSub (Bounds b) model
                
    }

let bordered events ui = initialize (borderedDefault ui) events