module Ui

open System
open Cmd
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows
open Geometry
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

type Ui<'e, 'm> = {
    init: 'm * Cmd<'e>
    view: 'm -> Drawing
    update: InterfaceEvent<'e> -> 'm -> 'm * Cmd<'e>
}

type InterfaceUpdate<'e, 'm> = InterfaceEvent<'e> -> 'm -> 'm * Cmd<'e>

type InterfaceModify<'e, 'm> = InterfaceUpdate<'e, 'm> -> 'm -> 'm * Cmd<'e>

let initialize ui events =
    let folder (m, cmd) e =
        let (m2, cmd2) = ui.update (Event e) m
        (m2, Cmd.batch [cmd; cmd2])
    { ui with init = events |> (ui.init |> List.fold folder)}

let sendEvents events update init =
    let folder (m, cmd) e =
        let (m2, cmd2) = update (Event e) m
        (m2, Cmd.batch [cmd; cmd2])
    events |> ((init, Cmd.none) |> List.fold folder)

let apply (init: 'm * Cmd<'e>) (ui: Ui<'e, 'm>) (events: 'e list) =
    let folder (m, cmd) e =
        let (m2, cmd2) = ui.update (Event e) m
        (m2, Cmd.batch [cmd; cmd2])
    events |> (init |> List.fold folder)

let onchange (update: 'm -> 'm -> 'm * Cmd<'e>) ui =
    let update e m =
        let (updated, cmd) = ui.update e m
        let (updated2, cmd2) = update m updated
        (updated2, Cmd.batch [cmd; cmd2])
    { ui with update = update }

let thenUpdate (m, cmd) update =
    let (m2, cmd2) = update m
    (m, Cmd.batch [cmd; cmd2])
