module Arranged

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui

type Layout = 
  { size: Size2F
    transform: Matrix3x2
    inverse: Matrix3x2
  }

type Arrange = Size2F -> Size2F -> Size2F * Layout

type Event =
  | Arrange of Arrange

type ArrangedModel<'m> = {
    layout: Layout
    arranged: ContentModel<'m>
}

let translateCrop vec outerSize innerSize =
    let layout =
      { size = innerSize
        transform = Matrix3x2.Translation(vec)
        inverse = Matrix3x2.Translation(-vec) }
    (outerSize, layout)

let center (outer: Size2F) (inner: Size2F) =
    let x = outer.Width / 2.0f - inner.Width / 2.0f
    let y = outer.Height / 2.0f - inner.Height / 2.0f
    translateCrop (Vector2(x, y)) outer inner

let margin thickness (outer: Size2F) (inner: Size2F) =
    let width = max (outer.Width - thickness * 2.0f) 0.0f
    let height = max (outer.Height - thickness * 2.0f) 0.0f
    translateCrop (Vector2(thickness, thickness)) outer (Size2F(width, height))

let padding thickness (outer: Size2F) (inner: Size2F) =
    let size = (Size2F(inner.Width + thickness * 2.0f, inner.Height + thickness * 2.0f))
    translateCrop (Vector2(thickness, thickness)) size inner

let arranged (arrange: Arrange) (ui: Interface<'e, 'm>): Interface<'e, ArrangedModel<'m>> = 
  { init = 
        let (sub, cmd) = ui.init
        let model =
            { bounds = Size2F.Zero
              content = 
                { arranged = sub
                  layout = 
                    { size = Size2F.Zero
                      transform = Matrix3x2.Identity
                      inverse = Matrix3x2.Identity } } }
        (model, cmd) 

    view =
        fun m t ->
            let old = t.Transform
            let tform = Matrix3x2.Multiply(Matrix3x2.op_Implicit(t.Transform), m.content.layout.transform)
            t.Transform <- Matrix3x2.op_Implicit(tform)
            t.PushAxisAlignedClip(
                RawRectangleF(
                    0.0f, 0.0f, 
                    m.content.layout.size.Width, m.content.layout.size.Height), 
                AntialiasMode.PerPrimitive)
            ui.view m.content.arranged t
            t.PopAxisAlignedClip()
            t.Transform <- old

    update =
        fun e m -> 
            let updateArrangement outer (inner, cmd) =
                if inner.bounds = outer.bounds then 
                    ({ outer with content = { outer.content with arranged = inner } }, cmd)
                else
                    let (outerSize, layout) = arrange outer.bounds inner.bounds
                    let (arrangedContent, arrangedCmd) = ui.update (Content (Bounds layout.size)) inner
                    let model = { bounds = outerSize; content = { arranged = arrangedContent; layout = layout } }
                    (model, Cmd.batch [cmd; arrangedCmd])

            match e with
            | Input (MouseMove p) -> 
                let pmapped = Matrix3x2.TransformPoint(m.content.layout.inverse, p)
                if pmapped.X >= 0.0f && pmapped.X <= m.content.layout.size.Width &&
                   pmapped.Y >= 0.0f && pmapped.Y <= m.content.layout.size.Height then
                    updateArrangement m (ui.update (Input (MouseMove pmapped)) m.content.arranged)
                else
                    updateArrangement m (ui.update (Input MouseLeave) m.content.arranged)

            | Content (Bounds s) -> 
                updateArrangement { m with bounds = s } (m.content.arranged, Cmd.none)
                
            | _ -> 
                updateArrangement m (ui.update e m.content.arranged)
  }

let translated vec = arranged (translateCrop vec)

let centered ui = arranged center ui

let margined thickness = arranged (margin thickness)

let padded thickness = arranged (padding thickness)