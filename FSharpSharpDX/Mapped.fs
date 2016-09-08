
module Mapped

open Ui

let map eventMap commandMap ui =
    { 
    init = 
        let (model, cmd) = ui.init
        (model, Cmd.map commandMap cmd)
    
    view = ui.view
    
    update =
        fun event model -> 
            let (model, cmd) =
                match event with
                | Event e -> ui.update (Event (eventMap e)) model
                | Input i -> ui.update (Input i) model
                | InterfaceEvent.Content c -> ui.update (InterfaceEvent.Content c) model
            (model, Cmd.map commandMap cmd)
    }

let mapMany eventMap commandMap ui =
    { 
    init = 
        let (model, cmd) = ui.init
        (model, Cmd.map commandMap cmd)
    
    view = ui.view
    
    update =
        fun event model -> 
            let (model, cmd) =
                match event with
                | Event e -> sendEvents (eventMap e) ui.update model
                | Input i -> ui.update (Input i) model
                | InterfaceEvent.Content c -> ui.update (InterfaceEvent.Content c) model
            (model, Cmd.map commandMap cmd)
    }