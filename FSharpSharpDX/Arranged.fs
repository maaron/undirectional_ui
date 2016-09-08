module Arranged

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui
open Mapped

type Layout = 
  { size: Size2F
    transform: Matrix3x2
    inverse: Matrix3x2
  }

type Arrange = Size2F -> Size2F -> Size2F * Layout

type Event<'e> =
  | Arrange of Arrange
  | Arranged of 'e

type ArrangedModel<'m> = {
    arrange: Arrange
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

let arranged (arrange: Arrange) (ui: Interface<'e, 'm>): Interface<Event<'e>, ArrangedModel<'m>> = 
  { init = 
        let (sub, cmd) = ui.init
        let model =
            { bounds = Size2F.Zero
              content = 
                { arrange = arrange
                  arranged = sub
                  layout = 
                    { size = Size2F.Zero
                      transform = Matrix3x2.Identity
                      inverse = Matrix3x2.Identity } } }
        (model, Cmd.map Arranged cmd) 

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
                    ({ outer with content = { outer.content with arranged = inner } }, Cmd.map Arranged cmd)
                else
                    let (outerSize, layout) = outer.content.arrange outer.bounds inner.bounds
                    let (arrangedContent, arrangedCmd) = ui.update (Content (Bounds layout.size)) inner
                    let model = { bounds = outerSize; content = { outer.content with arranged = arrangedContent; layout = layout } }
                    (model, [cmd; arrangedCmd] |> List.map (Cmd.map Arranged) |> Cmd.batch)

            match e with
            | Input (MouseMove p) -> 
                let pmapped = Matrix3x2.TransformPoint(m.content.layout.inverse, p)
                if pmapped.X >= 0.0f && pmapped.X <= m.content.layout.size.Width &&
                   pmapped.Y >= 0.0f && pmapped.Y <= m.content.layout.size.Height then
                    updateArrangement m (ui.update (Input (MouseMove pmapped)) m.content.arranged)
                else
                    updateArrangement m (ui.update (Input MouseLeave) m.content.arranged)

            | Input i ->
                updateArrangement m (ui.update (Input i) m.content.arranged)

            | Content (Bounds s) -> 
                updateArrangement { m with bounds = s } (m.content.arranged, Cmd.none)
            
            | Content c ->
                updateArrangement m (ui.update (Content c) m.content.arranged)

            | Event (Arrange arrange) -> 
                updateArrangement { m with content = { m.content with arrange = arrange } } (m.content.arranged, Cmd.none)
            
            | Event (Arranged e) -> 
                updateArrangement m (ui.update (Event e) m.content.arranged)
  }

let onsize (update: Size2F -> InterfaceModify<'e, 'm>) ui =
  { init = ui.init
    
    view = ui.view

    update =
        fun event model ->
            match event with
            | Content (Bounds s) -> update s ui.update model
            | _ -> ui.update event model
  }

let translated vec = arranged (translateCrop vec)

let centered ui = arranged center ui

let margined thickness = arranged (margin thickness)

type PaddedEvent<'e> =
  | Padding of float32
  | Padded of 'e

let padded thickness ui = 
    arranged (padding thickness) ui
 |> Mapped.map
        (fun e -> 
            match e with
            | Padding p -> Arrange (padding p)
            | Padded e -> Arranged e
        )
        (fun e ->
            match e with
            | Arrange e -> Padding 0.0f
            | Arranged e -> Padded e
        )
