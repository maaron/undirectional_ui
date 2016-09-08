module Mouseover

open Ui
open Augmented

let mouseovered ui =
    let aug event model =
        match event with
        | Input (MouseMove mouse) -> 
            let bounds = ui.bounds (snd model)
            let isOver = mouse.X >= 0.0f && mouse.X <= bounds.Width 
                        && mouse.Y >= 0.0f && mouse.Y <= bounds.Height
            (isOver, snd model)
            
        | Input MouseLeave -> (false, snd model)

        | _ -> model

    augmentModel false aug ui

let onmouseover update ui =
    mouseovered ui
 |> onAugmentChange update