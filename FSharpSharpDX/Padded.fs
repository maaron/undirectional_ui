module Padded

open Ui
open Arranged
open SharpDX

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
  | Padding of float32
  | Padded of 'e

let padded thickness ui = 
    Arranged.arranged (padding thickness) ui
 |> Mapped.map
        (fun e -> 
            match e with
            | Padding p -> Arranged.Arrange (padding p)
            | Padded e -> Arranged.Arranged e
        )
        (fun e ->
            match e with
            | Arranged.Arrange e -> Padding 0.0f
            | Arranged.Arranged e -> Padded e
        )
