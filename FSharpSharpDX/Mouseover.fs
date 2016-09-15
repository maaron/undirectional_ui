module Mouseover

open SharpDX
open Ui
open Augmented
open Geometry

let mouseovered ui =
    let aug event model =
        let ((wasOver, availableSize), uimodel) = model
        match event with
        | Input (MouseMove mouse) -> 
            let drawing = ui.view uimodel
            let isOver = 
                match drawing.clip with
                | Some clip -> Rectangle.containsPoint mouse clip
                | None -> Rectangle.containsPoint mouse (Rectangle.fromPoints Point.zero drawing.size)
            ((isOver, availableSize), uimodel)
            
        | Input MouseLeave -> ((false, availableSize), snd model)

        | Bounds b -> ((wasOver, b), uimodel)

        | _ -> model

    augmentModel (false, Point.zero) aug ui

let onmouseover update ui =
    mouseovered ui
 |> onAugmentChange (fun (isOver, size) -> update isOver)