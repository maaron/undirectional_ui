module Translated

open SharpDX
open Arranged
open Ui

type Event<'e> =
  | Translation of Vector2
  | Translated of 'e


let translate vec = 
    {
    limit = id
    arrange = 
        fun available desired ->
            translateLayout vec desired desired
    }

let translated vec ui = 
    arranged (translate vec) ui
 |> Mapped.map
        (fun e -> 
            match e with
            | Translation v -> Arrange (translate v)
            | Translated e -> Arranged e
        )
        (fun e ->
            match e with
            | Arrange e -> Translation (Vector2(0.0f, 0.0f))
            | Arranged e -> Translated e
        )
