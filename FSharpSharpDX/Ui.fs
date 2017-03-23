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
open View

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

type Ui<'e, 'm> = 
  { init: 'm * Cmd<'e>
    view: 'm -> View
    update: InterfaceEvent<'e> -> 'm -> 'm * Cmd<'e> }

let ofView view =
  { init = (), Cmd.none
    view = fun _ -> view
    update = fun _ _ -> (), Cmd.none }

type Update<'e, 's, 'r> = 's -> 'r * ('s * Cmd<'e>)

type UpdateBuilder() =
    member x.Return (a) = fun s -> a, (s, Cmd.none)

    member x.Bind (m: Update<'e, 's, 'a>, f: 'a -> Update<'e, 's, 'b>) = 
        fun s -> 
            let (r, (s', c')) = m s
            let (r', (s'', c'')) = f r s'
            (r', (s'', Cmd.batch [c'; c'']))

let update = UpdateBuilder()

let set s = fun _ -> (), (s, Cmd.none)

let get = fun s -> s, (s, Cmd.none)

let modify f = fun s -> (), (f s, Cmd.none)

let react f = fun s -> (), f s

let fire e = fun s -> (), (s, e)

let run m = fun s -> m s |> snd

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

let onchange (f: 'm -> 'm -> 'm * Cmd<'e>) (ui: Ui<'e, 'm>) =
    let f e = update {
        let! m = get
        do! react <| ui.update e
        do! react <| f m }

    { ui with update = f >> run }
