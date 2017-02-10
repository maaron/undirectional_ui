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

(*

State: s -> (s, a)
Writer: (a, [])
Reader: s -> a

s -> (s, [])

bind: (s, []) -> (s -> (s, [])) -> (s, [])

???: (s -> s) -> s -> (s, ())
???: (s -> (s, [])) -> s -> ((), s, [])

modify: (s -> (s, [])) -> s -> (a, (s, []))

*)

type UpdaterBuilder() =
    member x.Zero () = fun m -> (m, Cmd.none)

    member x.Delay (f) = f ()

    member x.Combine (ua, ub) = 
        fun m ->
            let (m', cmd) = ua m
            let (m'', cmd') = ub m'
            (m'', Cmd.batch [cmd; cmd'])

    member x.Return (m: 'm) = fun _ -> (m, Cmd.none)

    member x.ReturnFrom(u) = u

    member x.Bind (u, f) =
        fun m ->
            let (m', cmd) = u m
            let (m'', cmd') = f m' m'
            (m'', Cmd.batch [cmd; cmd'])

let unitize up = 
    fun s -> 
        let (s', cmd) = up s
        ((), s', cmd)

let execState m s = 
    let (_, s', cmd) = m s
    (s', cmd)

type StateBuilder() = 
    member x.Return (a) = fun s -> (a, s, Cmd.none)

    member x.Bind (ma, f) =
        fun s ->
            let (a, s', cmd) = ma s
            let (b, s'', cmd') = f a s'
            (b, s'', Cmd.batch [cmd; cmd'])

    member x.Combine (ma, mb) =
        fun s ->
            let (_, s', cmd) = ma s
            let (b, s'', cmd') = mb s'
            (b, s'', Cmd.batch [cmd; cmd'])

    member x.ReturnFrom (ma) = ma |> unitize

    member x.Delay (f) = f ()

let state = UpdaterBuilder()
let state2 = StateBuilder()

let get = fun m -> (m, Cmd.none)
let get2 = fun m -> (m, m, Cmd.none)

let set m' = fun m -> (m', Cmd.none)
let set2 m' = fun m -> ((), m', Cmd.none)

let modify f = fun m -> (f m, Cmd.none)
let modify2 f = fun m -> ((), f m, Cmd.none)

let fire e = fun m -> (m, e)

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

let onchangeOld (update: 'm -> 'm -> 'm * Cmd<'e>) ui =
    let update e m =
        let (updated, cmd) = ui.update e m
        let (updated2, cmd2) = update m updated
        (updated2, Cmd.batch [cmd; cmd2])
    { ui with update = update }

let onchange (update: 'm -> 'm -> 'm * Cmd<'e>) ui =
    let upd e =
        state {
            let! m = get
            return! ui.update e
            return! update m
        }
    { ui with update = upd }

let onchange2 (handler: 'm -> 'm -> 'm * Cmd<'e>) (ui: Ui<'e, 'm>) =
    let update e =
        state2 {
            let! m = get2
            return! ui.update e
            return! handler m
        }
    { ui with update = update >> execState }

let thenUpdate (m, cmd) update =
    let (m2, cmd2) = update m
    (m, Cmd.batch [cmd; cmd2])
