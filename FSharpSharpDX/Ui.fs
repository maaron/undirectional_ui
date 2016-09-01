module Ui

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

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

type Component<'e, 'm> = {
    init: 'm * Cmd<'e>
    view: 'm -> Render
    update: 'e -> 'm -> 'm * Cmd<'e>
}

type ResourceEvent =
    | Create of RenderTarget
    | Release

type ContentEvent =
    | Bounds of Size2F
    | Resource of ResourceEvent

type ContentModel<'m> = {
    bounds: Size2F
    content: 'm
}

type InputEvent = 
    | MouseMove of Vector2
    | MouseEnter
    | MouseLeave
    | LeftButton of bool
    | RightButton of bool

type InterfaceEvent<'e> =
    | Input of InputEvent
    | Content of ContentEvent
    | Event of 'e

//type Interface<'e, 'm, 'c> = Component<InterfaceEvent<'e>, ContentModel<'m>>
type Interface<'e, 'm, 'c> = {
    init: ContentModel<'m> * Cmd<'c>
    view: ContentModel<'m> -> Render
    update: InterfaceEvent<'e> -> ContentModel<'m> -> ContentModel<'m> * Cmd<'c>
}

type InterfaceUpdate<'e, 'm, 'c> = InterfaceEvent<'e> -> ContentModel<'m> -> ContentModel<'m> * Cmd<'c>

type ResourceModel<'p, 'r> = {
    properties: 'p
    resource: 'r option
}

let initialize ui events =
    let folder (m, cmd) e =
        let (m2, cmd2) = ui.update (Event e) m
        (m2, Cmd.batch [cmd; cmd2])
    { ui with init = events |> (ui.init |> List.fold folder)}

let apply (init: ContentModel<'m> * Cmd<'c>) (ui: Interface<'e, 'm, 'c>) (events: 'e list) =
    let folder (m, cmd) e =
        let (m2, cmd2) = ui.update (Event e) m
        (m2, Cmd.batch [cmd; cmd2])
    events |> (init |> List.fold folder)

let augmentModel (init: 'm2) (update: InterfaceEvent<'e> -> ('m2 * ContentModel<'m>) -> ('m2 * ContentModel<'m>)) (ui: Interface<'e, 'm, 'c>): Interface<'e, 'm2 * 'm, 'c> =
  { init = 
        let (sub, cmd) = ui.init
        let model = { bounds = sub.bounds; content = (init, sub.content) }
        (model, cmd)
    
    view = fun m -> ui.view { bounds = m.bounds; content = snd m.content }
    
    update = 
        fun e mAug -> 
            let m = { bounds = mAug.bounds; content = snd mAug.content }
            let (mUpdate, cmd) = ui.update e m
            let (mAugUpdate, cmUpdate) = update e (fst mAug.content, mUpdate)
            ({ bounds = cmUpdate.bounds; content = (mAugUpdate, cmUpdate.content) }, cmd)
  }

let augment (init: 'm2) (update: InterfaceUpdate<'e, 'm, 'c> -> InterfaceEvent<'e> -> ('m2 * ContentModel<'m>) -> ('m2 * ContentModel<'m>) * Cmd<'c>) (ui: Interface<'e, 'm, 'c>): Interface<'e, 'm2 * 'm, 'c> =
  { init = 
        let (sub, cmd) = ui.init
        let model = { bounds = sub.bounds; content = (init, sub.content) }
        (model, cmd)
    
    view = fun m -> ui.view { bounds = m.bounds; content = snd m.content }
    
    update = 
        fun e { bounds = bounds; content = (b, content) } -> 
            let m = { bounds = bounds; content = content }
            let ((bnew, contentnew), cmd) = update ui.update e (b, m)
            ({ bounds = contentnew.bounds; content = (bnew, contentnew.content) }, cmd)
  }

let mouseovered ui =
    let aug e (b, m) =
        match e with
        | Input (MouseMove mouse) -> 
            let isOver = mouse.X >= 0.0f && mouse.X <= m.bounds.Width 
                        && mouse.Y >= 0.0f && mouse.Y <= m.bounds.Height
            (isOver, m)
            
        | Input MouseLeave -> (false, m)

        | _ -> (b, m)

    augmentModel false aug ui

let onchange (update: ContentModel<'m> -> ContentModel<'m> -> ContentModel<'m> * Cmd<'e>) ui =
    let update e m =
        let (updated, cmd) = ui.update e m
        let (updated2, cmd2) = update m updated
        (updated2, Cmd.batch [cmd; cmd2])
    { ui with update = update }

// Should find a way to take the mouseover state back out???
let onmouseover2 enter leave ui =
    let withMouse = mouseovered ui
    withMouse |> onchange 
      ( fun prev next -> 
            let isOver = fst next.content
            let wasOver = fst prev.content
            let next2 = 
                if wasOver = isOver then 
                    (next, Cmd.none)
                else
                    apply (next, Cmd.none) withMouse (if isOver then enter else leave)
            next2
      )

// This is an example of a combinator that generates new events that only travel forward within 
// the local context.  This is in contrast to commands, which indirectly cause events to arrive 
// at the application root.  This is useful in cases where we don't need to be aware of the event
// outside of the context of the combinator, i.e., combinators upstream from this one won't 
// receive the event.
let onmouseover handler =
    augment false 
        (fun update e (b, m) -> 
            let updateMouseOver isOver =
                let (normalUpdate, cmd) = update e m
                let (mouseUpdate, cmds) = 
                    if not isOver = b then 
                        let (sub, cmd2) = update (Event (handler isOver)) normalUpdate
                        (sub, Cmd.batch [cmd; cmd2])
                    else (normalUpdate, cmd)
                ((isOver, mouseUpdate), cmds)

            match e with
            | Input (MouseMove mouse) -> 
                let isOver = mouse.X >= 0.0f && mouse.X <= m.bounds.Width 
                            && mouse.Y >= 0.0f && mouse.Y <= m.bounds.Height
                updateMouseOver isOver
            
            | Input MouseLeave ->
                updateMouseOver false

            | _ -> 
                let (updated, cmd) = update e m
                ((b, updated), cmd)
        )

