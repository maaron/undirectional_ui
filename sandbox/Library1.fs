
module Ui

open Geometry
open Cmd
open Draw
open Draw.Drawing

type InputEvent = 
    | MouseMove of Point
    | MouseEnter
    | MouseLeave
    | LeftButton of bool
    | RightButton of bool

type InterfaceEvent<'e> =
    | Input of InputEvent
    | Bounds of Point
    | Event of 'e

type Ui<'e, 'm, 's> =
  { init: ('m * 's) * Cmd<'e>
    update: InterfaceEvent<'e> -> 'm * 's -> ('m * 's) * Cmd<'e>
    view: ('m * 's) -> Drawing
  }

