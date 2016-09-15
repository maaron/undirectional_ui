module Translated

open Ui
open Arranged
open Geometry

let translate vec = 
    {
    limit = id
    arrange = 
        fun available desired ->
            translateLayout vec desired { topLeft = Point.zero; bottomRight = desired }
    }

let translated vec ui = 
    arranged (translate vec) ui