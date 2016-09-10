module Mouseover

open SharpDX
open Ui
open Augmented

let mouseovered ui =
    let aug event model =
        let ((wasOver, availableSize), uimodel) = model
        match event with
        | Input (MouseMove mouse) -> 
            let bounds = ui.bounds availableSize (snd model)
            let isOver = mouse.X >= 0.0f && mouse.X < bounds.Width 
                        && mouse.Y >= 0.0f && mouse.Y < bounds.Height
            ((isOver, availableSize), snd model)
            
        | Input MouseLeave -> ((false, availableSize), snd model)

        | Bounds b -> ((wasOver, b), uimodel)

        | _ -> model

    augmentModel (false, Size2F.Zero) aug ui

let onmouseover update ui =
    mouseovered ui
 |> onAugmentChange (fun (isOver, size) -> update isOver)