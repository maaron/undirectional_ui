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

// This type is used to render arranged UI's, as well as map input coordinates to the arranged 
// UI's transformed coordinate space.
type Layout = 
    {
    // Indicates the overall size of the arranged UI.  For example, for the padded combinator, 
    // this size will be larger than the clip by an amout equal to the padding.  The margined
    // combinator, on the other hand, will set this equal to the available size minus the margin.
    bounds: Size2F

    // Indicates the size of the rectangle used to clip the arranged UI's content
    clip: Size2F

    // Indicates the transform to apply to the arranged UI's content
    transform: Matrix3x2

    // Indicates the reverse transform, used to map mouse coordinates to the arranged UI's 
    // transformed coordinate space
    inverse: Matrix3x2
    }

// This type is used to control how layout is performed.  It is composed of two functions, 
// "limit", amd "arrange".  Limit is passed the total bounds of the containing UI, and returns 
// the portion of that size that is passed to the UI being arranged to calculate its size.  
// Arrange is a function that takes the total bounds and the arranged UI's calculated size and 
// returns the layout.
type Arranger = 
    {
    limit: Size2F -> Size2F
    arrange: Size2F -> Size2F -> Layout
    }

let translateLayout vec bounds clip =
    {
    bounds = bounds
    clip = clip
    transform = Matrix3x2.Translation(vec)
    inverse = Matrix3x2.Translation(-vec)
    }

let translate vec = 
    {
    limit = id
    arrange = 
        fun available desired ->
            translateLayout vec desired desired
    }

let center =
    {
    limit = id
    arrange = 
        fun available desired ->
            let x = available.Width / 2.0f - desired.Width / 2.0f
            let y = available.Height / 2.0f - desired.Height / 2.0f
            translateLayout (Vector2(x, y)) available desired
    }

let margin thickness =
    {
    limit = 
        fun available -> 
            let doubleThickness = thickness * 2.0f
            Size2F(available.Width - doubleThickness, available.Height - doubleThickness)

    arrange = 
        fun available desired ->
            translateLayout (Vector2(thickness, thickness)) available desired
    }

let padding thickness =
    let doubleThickness = thickness * 2.0f
    {
    limit = 
        fun available -> 
            Size2F(
                max (available.Width - doubleThickness) 0.0f,
                max (available.Height - doubleThickness) 0.0f)

    arrange =
        fun available desired ->
            let bounds = Size2F(desired.Width + doubleThickness, desired.Height + doubleThickness)
            translateLayout (Vector2(thickness, thickness)) bounds desired
    }

type Event<'e> =
  | Arrange of Arranger
  | Arranged of 'e

type Model<'m> = {
    arranger: Arranger
    bounds: Size2F
    layout: Layout
    arranged: 'm
}

let arranged (arranger: Arranger) (ui: Ui<'e, 'm>): Ui<Event<'e>, Model<'m>> = 
  { init = 
        let (sub, cmd) = ui.init
        let bounds = arranger.limit Size2F.Zero
        let model =
            {
            bounds = bounds
            arranger = arranger
            arranged = sub
            layout = arranger.arrange bounds (ui.bounds bounds sub)
            }
        (model, Cmd.map Arranged cmd) 

    bounds = fun size model -> model.layout.bounds

    view =
        fun m t ->
            let old = t.Transform
            let tform = Matrix3x2.Multiply(Matrix3x2.op_Implicit(t.Transform), m.layout.transform)
            t.Transform <- Matrix3x2.op_Implicit(tform)
            t.PushAxisAlignedClip(
                RawRectangleF(
                    0.0f, 0.0f, 
                    m.layout.bounds.Width, m.layout.bounds.Height), 
                AntialiasMode.PerPrimitive)
            ui.view m.arranged t
            t.PopAxisAlignedClip()
            t.Transform <- old

    update =
        fun e m -> 
            let updateArrangement model (inner, cmd) =
                let limit = model.arranger.limit model.bounds
                let desired = ui.bounds limit inner
                let layout = model.arranger.arrange limit desired
                let (arrangedContent, arrangedCmd) = ui.update (Bounds layout.clip) inner
                let updatedModel = { model with arranged = arrangedContent; layout = layout }
                (updatedModel, [cmd; arrangedCmd] |> List.map (Cmd.map Arranged) |> Cmd.batch)

            let updateArrangementIfChanged model (inner, cmd) =
                let innerBounds = ui.bounds model.layout.clip inner
                if innerBounds = model.layout.clip then 
                    ({ model with arranged = inner }, Cmd.map Arranged cmd)
                else updateArrangement model (inner, cmd)

            match e with
            | Input (MouseMove p) -> 
                let pmapped = Matrix3x2.TransformPoint(m.layout.inverse, p)
                if pmapped.X >= 0.0f && pmapped.X <= m.layout.bounds.Width &&
                   pmapped.Y >= 0.0f && pmapped.Y <= m.layout.bounds.Height then
                    updateArrangement m (ui.update (Input (MouseMove pmapped)) m.arranged)
                else
                    updateArrangement m (ui.update (Input MouseLeave) m.arranged)

            | Input i ->
                updateArrangementIfChanged m (ui.update (Input i) m.arranged)

            | Bounds s -> 
                updateArrangement { m with bounds = s } (m.arranged, Cmd.none)
            
            | Resource r ->
                updateArrangementIfChanged m (ui.update (Resource r) m.arranged)

            | Event (Arrange arrange) -> 
                updateArrangementIfChanged { m with arranger = arranger } (m.arranged, Cmd.none)
            
            | Event (Arranged e) -> 
                updateArrangementIfChanged m (ui.update (Event e) m.arranged)
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
