module Augmented

open Ui

type Augmented<'e, 'm, 'a> = Ui<'e, 'a * 'm>

let augmentModel initAug update ui =
  { init = 
        let (sub, cmd) = ui.init
        ((initAug, sub), cmd)

    bounds = fun (aug, model) -> ui.bounds model
    
    view = fun (aub, model) -> ui.view model
    
    update = 
        fun event (aug, model) -> 
            let (model2, cmd) = ui.update event model
            (update event (aug, model2), cmd)
  }

let onAugmentChange (getUpdate: 'a -> InterfaceModify<'e, 'a * 'm>) (ui: Augmented<'e, 'm, 'a>) =
    let update event model =
        let (model2, cmd) = ui.update event model
        if not (fst model = fst model2) 
            then 
                let (model3, changecmd) = getUpdate (fst model2) ui.update model2
                (model3, Cmd.batch [cmd; changecmd])
            else (model2, Cmd.none)
        
    { ui with update = update }

let updateAugmented (update: InterfaceEvent<'e> -> 'a -> 'a * Cmd<'e>) (ui: Ui<'e, 'a * 'b>) =
    ui.update 