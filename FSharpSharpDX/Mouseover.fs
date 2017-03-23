module Mouseover

open SharpDX
open Ui
open Augmented
open Geometry
open View

let mouseovered ui =
    let aug event model =
        let ((wasOver, availableSize), uimodel) = model
        match event with
        | Input (MouseMove mouse) -> 
            let isOver = 
                ui.view uimodel |> clip |> Rectangle.containsPoint mouse
            ((isOver, availableSize), uimodel)
            
        | Input MouseLeave -> ((false, availableSize), snd model)

        | Bounds b -> ((wasOver, b), uimodel)

        | _ -> model

    augmentModel (false, Point.zero) aug ui

let onmouseover update ui =
    mouseovered ui
 |> onAugmentChange (fun (isOver, size) -> update isOver)

let viewmodel ui =
    let addview (m, c) = (m, ui.view m), c
    let init = ui.init |> addview
    
    let view (m, v) = v

    let update e (m, v) = ui.update e m |> addview

    { init = init; view = view; update = update }
