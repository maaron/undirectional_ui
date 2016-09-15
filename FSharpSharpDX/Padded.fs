module Padded

open SharpDX
open Geometry
open Ui
open Cmd
open Arranged

let padding thickness =
    let doubleThickness = thickness * 2.0f
    {
    limit = 
        fun available -> 
            { 
                x = max (available.x - doubleThickness) 0.0f
                y = max (available.y - doubleThickness) 0.0f
            }

    arrange =
        fun available desired ->
            let bounds = { x = desired.x + doubleThickness; y = desired.y + doubleThickness }
            translateLayout (Vector2(thickness, thickness)) bounds { topLeft = Point.zero; bottomRight = desired }
    }

let padded thickness ui = 
    arranged (padding thickness) ui