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
    {
    outer: Size2F
    inner: Size2F
    transform: Matrix3x2
    inverse: Matrix3x2
    }

type Arrange = Size2F -> Size2F -> Layout

type Event<'e> =
  | Arrange of Arrange
  | Arranged of 'e

type Model<'m> = {
    arrange: Arrange
    bounds: Size2F
    layout: Layout
    arranged: 'm
}

let translateCrop vec outerSize innerSize =
    { 
    outer = outerSize
    inner = Geometry.minSize innerSize outerSize
    transform = Matrix3x2.Translation(vec)
    inverse = Matrix3x2.Translation(-vec)
    }

let translate vec outer inner = 
    translateCrop vec inner outer

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

let arranged (arrange: Arrange) (ui: Ui<'e, 'm>): Ui<Event<'e>, Model<'m>> = 
  { init = 
        let (sub, cmd) = ui.init
        let model =
            {
            bounds = Size2F.Zero
            arrange = arrange
            arranged = sub
            layout = arrange Size2F.Zero (ui.bounds sub)
            }
        (model, Cmd.map Arranged cmd) 

    bounds = fun m -> m.layout.outer

    view =
        fun m t ->
            let old = t.Transform
            let tform = Matrix3x2.Multiply(Matrix3x2.op_Implicit(t.Transform), m.layout.transform)
            t.Transform <- Matrix3x2.op_Implicit(tform)
            t.PushAxisAlignedClip(
                RawRectangleF(
                    0.0f, 0.0f, 
                    m.layout.inner.Width, m.layout.inner.Height), 
                AntialiasMode.PerPrimitive)
            ui.view m.arranged t
            t.PopAxisAlignedClip()
            t.Transform <- old

    update =
        fun e m -> 
            let updateArrangement outer (inner, cmd) =
                let innerBounds = ui.bounds inner
                if innerBounds = outer.layout.inner then 
                    ({ outer with arranged = inner }, Cmd.map Arranged cmd)
                else
                    let layout = outer.arrange outer.bounds innerBounds
                    let (arrangedContent, arrangedCmd) = ui.update (Bounds layout.inner) inner
                    let model = { outer with arranged = arrangedContent; layout = layout }
                    (model, [cmd; arrangedCmd] |> List.map (Cmd.map Arranged) |> Cmd.batch)

            match e with
            | Input (MouseMove p) -> 
                let pmapped = Matrix3x2.TransformPoint(m.layout.inverse, p)
                if pmapped.X >= 0.0f && pmapped.X <= m.layout.inner.Width &&
                   pmapped.Y >= 0.0f && pmapped.Y <= m.layout.inner.Height then
                    updateArrangement m (ui.update (Input (MouseMove pmapped)) m.arranged)
                else
                    updateArrangement m (ui.update (Input MouseLeave) m.arranged)

            | Input i ->
                updateArrangement m (ui.update (Input i) m.arranged)

            | Bounds s -> 
                updateArrangement { m with bounds = s } (m.arranged, Cmd.none)
            
            | Resource r ->
                updateArrangement m (ui.update (Resource r) m.arranged)

            | Event (Arrange arrange) -> 
                updateArrangement { m with arrange = arrange } (m.arranged, Cmd.none)
            
            | Event (Arranged e) -> 
                updateArrangement m (ui.update (Event e) m.arranged)
  }

let onsize (update: Size2F -> InterfaceModify<'e, 'm>) ui =
  { init = ui.init

    bounds = ui.bounds
    
    view = ui.view

    update =
        fun event model ->
            match event with
            | Bounds s -> update s ui.update model
            | _ -> ui.update event model
  }

let translated vec = arranged (translate vec)

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
