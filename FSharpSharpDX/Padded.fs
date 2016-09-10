module Padded

open Ui
open SharpDX

let padding thickness (outer: Size2F) (inner: Size2F) =
    let size = (Size2F(inner.Width + thickness * 2.0f, inner.Height + thickness * 2.0f))
    Arranged.translateCrop (Vector2(thickness, thickness)) size inner

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
