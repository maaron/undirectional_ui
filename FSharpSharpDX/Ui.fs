module Ui

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows
open Geometry

type Render = RenderTarget -> unit

type Cmd<'e> = 'e list * ((unit -> unit) -> unit)

module Cmd =
    let none: Cmd<'a> = 
        ([], ignore)

    let map (f: 'a -> 'b) (cmd: Cmd<'a>): Cmd<'b> = 
        (List.map f (fst cmd), snd cmd)

    let batch (cmds: Cmd<'a> list): Cmd<'a> = 
        let events = [for cmd in cmds do 
                      for a in (fst cmd) do 
                      yield a]
        let start callback = for c in cmds do snd c callback
        (events, start)

type ResourceEvent =
    | Create of RenderTarget
    | Release

type InputEvent = 
    | MouseMove of Vector2
    | MouseEnter
    | MouseLeave
    | LeftButton of bool
    | RightButton of bool

type InterfaceEvent<'e> =
    | Input of InputEvent
    | Bounds of Size2F
    | Resource of ResourceEvent
    | Event of 'e

type Ui<'e, 'm> = {
    init: 'm * Cmd<'e>
    bounds: 'm -> Size2F
    view: 'm -> Render
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
