module Augmented

open Ui

type Augmented<'e, 'm, 'a> = Interface<'e, 'a * 'm>

let augmentModel init update ui =
  { init = 
        let (sub, cmd) = ui.init
        let model = { bounds = sub.bounds; content = (init, sub.content) }
        (model, cmd)
    
    view = fun m -> ui.view { bounds = m.bounds; content = snd m.content }
    
    update = 
        fun event model -> 
            let uimodel = { bounds = model.bounds; content = snd model.content }
            let (uimodel2, uicmd) = ui.update event uimodel
            let model2 = update event { bounds = uimodel2.bounds; content = (fst model.content, uimodel2.content) }
            (model2, uicmd)
  }

let augmentContent init update ui =
  { init = 
        let (sub, cmd) = ui.init
        let model = { bounds = sub.bounds; content = (init, sub.content) }
        (model, cmd)
    
    view = fun m -> ui.view { bounds = m.bounds; content = snd m.content }
    
    update = 
        fun event model -> 
            let uimodel = { bounds = model.bounds; content = snd model.content }
            let (uimodel2, cmd) = ui.update event uimodel
            let augcontent = update event (fst model.content, uimodel2.content)
            ({ bounds = uimodel2.bounds; content = augcontent }, cmd)
  }

let onAugmentChange (getUpdate: 'a -> InterfaceModify<'e, 'a * 'm>) (ui: Augmented<'e, 'm, 'a>) =
    let update event model =
        let (model2, cmd) = ui.update event model
        if not (fst model.content = fst model2.content) 
            then 
                let (model3, changecmd) = getUpdate (fst model2.content) ui.update model2
                (model3, Cmd.batch [cmd; changecmd])
            else (model2, Cmd.none)
        
    { ui with update = update }

let updateAugmented (update: InterfaceEvent<'e> -> 'a -> 'a * Cmd<'e>) (ui: Interface<'e, 'a * 'b>) =
    ui.update 