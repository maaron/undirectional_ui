module Mouseover

open Ui
open Augmented

let mouseovered ui =
    let aug event model =
        match event with
        | Input (MouseMove mouse) -> 
            let isOver = mouse.X >= 0.0f && mouse.X <= model.bounds.Width 
                        && mouse.Y >= 0.0f && mouse.Y <= model.bounds.Height
            { model with content = (isOver, snd model.content) }
            
        | Input MouseLeave -> { model with content = (false, snd model.content) }

        | _ -> model

    augmentModel false aug ui

let onmouseover update ui =
    mouseovered ui
 |> onAugmentChange update