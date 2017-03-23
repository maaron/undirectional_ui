module Cmd

type Cmd<'e> = 'e list * ((unit -> unit) -> unit)

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

