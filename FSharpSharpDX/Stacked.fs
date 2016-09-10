
module Stacked

open SharpDX
open Translated
open Ui

type Event<'e, 'p> =
  | List of 'p list
  | Item of int * 'e

type Model<'e, 'm> = Ui<'e, Arranged.Model<'m>> * Arranged.Model<'m> list

let stacked (template: 'p -> Ui<'e, 'm>) =
    {
    init = ([], Cmd.none)

    bounds = 
        fun size model -> 
            model 
         |> List.fold 
                (fun accum (ui, item) -> 
                    let itemSize = ui.bounds size item
                    Size2F(
                        max accum.Width itemSize.Width,
                        accum.Height + itemSize.Height)
                )
                (Size2F(0.0f, 0.0f))

    view = 
        let renderItem target (ui, model) = ui.view model target
        fun model target -> model |> List.iter (renderItem target)

    update =
        // TODO: This still needs to detect bounds changes in items and update layouts of 
        // subsequent items for all events.  Perhaps all the updates can be factored into a single 
        // function that performs layout.
        fun event model ->
            let mapCmd index cmd = Cmd.map (fun e -> Item (index, e)) cmd

            let arrangeItems items = 
                let uis = 
                    items 
                 |> Seq.map template

                let (model, (height, cmd)) = 
                    uis
                 |> Seq.mapFold 
                      ( fun ((i, offset), cmds) ui -> 
                            let arranged = translated (Vector2(0.0f, offset)) ui
                            let (model, cmd) = arranged.init
                            let bounds = arranged.bounds model.bounds model
                            let result = (arranged, model)
                            (result, ((i, offset + bounds.Height), Cmd.batch [cmds; mapCmd i cmd]))
                      )
                      ((0, 0.0f), Cmd.none)
                
                (Seq.toList model, cmd)

            let updateItem model index e =
                model 
             |> List.mapi 
                  ( fun i (ui, itemModel) -> 
                        if i = index then 
                            let (updated, cmd) = ui.update e itemModel
                            (ui, (updated, mapCmd i cmd))
                        else
                            (ui, (itemModel, Cmd.none))
                  )
             |> List.mapFold
                  ( fun cmds (ui, (model, cmd)) -> ((ui, model), Cmd.batch [cmds; cmd]))
                  Cmd.none

            let broadcast e model =
                let (items, (count, cmds)) =
                    model
                 |> List.mapFold
                      ( fun (i, cmds) (ui, itemModel) -> 
                            let (updated, cmd) = ui.update e itemModel
                            ((ui, updated), (i + 1, Cmd.batch [cmds; mapCmd i cmd])))
                      (0, Cmd.none)
                (items, cmds)

            match event with
            | Event (List items) -> arrangeItems items
            | Event (Item (index, e)) -> updateItem model index (Event e)
            | Input i -> broadcast (Input i) model
            | Bounds b -> broadcast (Bounds b) model
            | Resource r -> broadcast (Resource r) model
    }