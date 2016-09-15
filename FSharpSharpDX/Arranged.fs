module Arranged

open System

open Ui
open Cmd
open SharpDX
open Geometry
open Draw.Drawing
open Draw.Primitive

// This type is used to render arranged UI's, as well as map input coordinates to the arranged 
// UI's transformed coordinate space.
type Arrangement = 
    {
    // Indicates the overall size of the arranged UI.  For example, for the padded combinator, 
    // this size will be larger than the clip by an amout equal to the padding.  The margined
    // combinator, on the other hand, will set this equal to the available size minus the margin.
    size: Point

    // Indicates the size of the rectangle used to clip the arranged UI's content
    clip: Rectangle option

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
type Layout = 
    {
    limit: Point -> Point
    arrange: Point -> Point -> Arrangement
    }

let translateLayout vec size clip =
    { 
    size = size
    clip = Some clip
    transform = Matrix3x2.Translation(vec)
    inverse = Matrix3x2.Translation(-vec)
    }

let center =
    {
    limit = id
    arrange = 
        fun available desired ->
            let x = available.x / 2.0f - desired.x / 2.0f
            let y = available.y / 2.0f - desired.y / 2.0f
            translateLayout 
                (Vector2(x, y)) 
                available 
                { topLeft = Point.zero; bottomRight = desired }
    }

let margin thickness =
    {
    limit = 
        fun available -> 
            let doubleThickness = thickness * 2.0f
            { x = available.x - doubleThickness; y = available.y - doubleThickness }

    arrange = 
        fun available desired ->
            translateLayout 
                (Vector2(thickness, thickness)) 
                available 
                { topLeft = Point.zero; bottomRight = desired }
    }

let identityLayout size =
    {
    size = size
    clip = None
    transform = Matrix3x2.Identity
    inverse = Matrix3x2.Identity
    }

let identityArranger =
    {
    limit = id
    arrange = fun available desired -> identityLayout desired
    }

type Model<'m> = {
    available: Point
    arrangement: Arrangement
    child: 'm * Drawing
}

let arranged (arranger: Layout) (ui: Ui<'e, 'm>): Ui<'e, Model<'m>> = 
  { init = 
        let (child, cmd) = ui.init
        let model =
            {
            available = Point.zero
            arrangement = 
                {
                    size = Point.zero
                    clip = None
                    transform = Matrix3x2.Identity
                    inverse = Matrix3x2.Identity
                }
            child = (child, ui.view child)
            }
        (model, cmd) 

    view =
        fun model -> 
            {
                size = model.arrangement.size
                clip = model.arrangement.clip
                transform = model.arrangement.transform
                commands = [Command.Drawing (snd model.child)]
            }

    update =
        fun e m -> 
            let updateArrangement model (child, cmd) =
                let drawing = ui.view child
                (
                    { model with 
                        arrangement = arranger.arrange model.available drawing.size
                        child = (child, drawing)
                    },
                    cmd
                )

            match e with
            | Input (MouseMove p) -> 
                let pmapped = Point.transform m.arrangement.inverse p

                let isClipped = 
                    match m.arrangement.clip with
                    | Some r -> Rectangle.containsPoint pmapped r |> not
                    | None -> false
                
                let mappedEvent =
                    if isClipped && 
                       Rectangle.containsPoint pmapped (Rectangle.fromPoints Point.zero m.arrangement.size) then
                        Input (MouseMove pmapped)
                    else
                        Input MouseLeave

                updateArrangement m (ui.update mappedEvent (fst m.child))

            | Bounds s -> 
                let limit = arranger.limit s
                if limit = m.available then (m, Cmd.none)
                else
                    updateArrangement { m with available = s } (ui.update (Bounds limit) (fst m.child))
            
            | _ -> 
                updateArrangement m (ui.update e (fst m.child))
  }

let onsize (update: Point -> InterfaceModify<'e, 'm>) ui =
  { init = ui.init

    view = ui.view

    update =
        fun event model ->
            match event with
            | Bounds s -> update s ui.update model
            | _ -> ui.update event model
  }

let centered ui = arranged center ui

let margined thickness = arranged (margin thickness)
