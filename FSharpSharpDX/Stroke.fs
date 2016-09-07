module Stroke

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui
open Brush
open StrokeStyle
open Resource

type Resource = Brush.Model * StrokeStyle.Model option

type Model =
  { brush: Brush.Model
    width: float32
    style: StrokeStyle.Model option
  }

type Event =
  | Brush of Brush.Event
  | Width of float32
  | Style of StrokeStyle.Event option

let init =
  { brush = Brush.init
    width = 0.0f
    style = None
  }

let release model =
  { model with 
        brush = Brush.release model.brush
        style = model.style |> Option.map (fun s -> StrokeStyle.release s) }

let create model target =
  { model with 
        brush = Brush.create model.brush target 
        style = model.style |> Option.map (fun s -> StrokeStyle.create s target.Factory) }

let update event model =
    match event with
    | Brush b -> { model with brush = Brush.update b model.brush }
    | Width w -> { model with width = w }
    | Style s -> 
        let updated =
          match (model.style, s) with
          | (None, None) -> None
          | (Some s, None) -> StrokeStyle.release s |> ignore; None
          | (None, Some e) -> Some (StrokeStyle.update e StrokeStyle.init)
          | (Some s, Some e) -> Some (StrokeStyle.update e s)
        { model with style = updated }