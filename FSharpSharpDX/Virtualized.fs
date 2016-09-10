
module Virtualized

open SharpDX
open Arranged
open Translated
open Ui

type Event<'e, 'p> =
  | Items of 'p list
  | Item of int * 'e

type Model<'e, 't, 'm> = 
    {
    bounds: Size2F
    size: Size2F
    items: 't list
    uis: (Ui<Arranged.Event<'e>, Arranged.Model<'m>> * Arranged.Model<'m>) list
    }

type Subtractor = Size2F -> (Size2F * Size2F)
type Reducer = Size2F -> Size2F -> Arranger * Subtractor
type Generator<'t, 'e, 'm> = 't -> Ui<'e, 'm>

let virtualized reducer (template: Generator<'t, 'e, 'm>) =
    {
    init = 
        (
            {
            bounds = Size2F.Zero
            size = Size2F.Zero
            items = []
            uis = []
            },
            Cmd.none
        )

    bounds = 
        fun size model -> model.size

    view = 
        let renderItem target (ui, itemModel) = ui.view itemModel target
        fun model target -> model.uis |> List.iter (renderItem target)

    update =
        fun event model ->
            let mapCmd index cmd = Cmd.map (fun e -> Item (index, e)) cmd

            let arrangeItem arranger item =
                let arranged = arranged arranger item
                let (model, cmd) = arranged.init
                let size = model.layout.bounds
                (arranged, model, cmd, size)

            let arrangeItems items = 
                let opt =
                    items
                 |> Seq.scan
                      ( fun (uis, index, used, (remaining: Size2F), cmds, isDone) item ->
                            let (arranger, subtractor) = reducer used remaining
                            let (itemUi, itemModel, cmd, size) = arrangeItem arranger (template item)
                            let (newUsed, newRemaining) = subtractor size
                            (
                                (itemUi, itemModel) :: uis, 
                                index + 1, 
                                newUsed, 
                                newRemaining,
                                Cmd.batch [cmds; mapCmd index cmd],
                                newRemaining.Width > 0.0f && newRemaining.Height > 0.0f
                            )
                      )
                      ([], 0, Size2F.Zero, model.bounds, Cmd.none, false)
                 |> Seq.takeWhile (fun (_, _, _, _, _, isDone) -> not isDone)
                 |> Seq.tryLast

                match opt with
                | Some (uis, count, used, remaining, cmd, isDone) ->
                    (
                        {
                        bounds = model.bounds
                        size = used
                        items = items
                        uis = List.rev uis
                        },
                        cmd
                    )
                | None -> ({ bounds = Size2F.Zero; size = Size2F.Zero; items = items; uis = []}, Cmd.none)

            let updateItem model index e =
                let (uis, cmd) =
                    model.uis 
                 |> List.mapi 
                      ( fun i (ui, itemModel) -> 
                            if i = index then 
                                let (updated, cmd) = ui.update e itemModel
                                (ui, (updated, mapCmd i cmd))
                            else
                                (ui, (itemModel, Cmd.none))
                      )
                 |> List.mapFold
                      ( fun cmds (ui, (model, cmd)) -> 
                            ((ui, model), Cmd.batch [cmds; cmd])
                      )
                      Cmd.none
                ({ model with uis = uis}, cmd)

            let broadcast e model =
                let (items, (count, cmds)) =
                    model.uis
                 |> List.mapFold
                      ( fun (i, cmds) (ui, itemModel) -> 
                            let (updated, cmd) = ui.update e itemModel
                            ((ui, updated), (i + 1, Cmd.batch [cmds; mapCmd i cmd])))
                      (0, Cmd.none)
                ({ model with uis = items }, cmds)

            match event with
            | Event (Items items) -> arrangeItems items
            | Event (Item (index, e)) -> updateItem model index (Event e)
            | Input i -> broadcast (Input i) model
            | Bounds b -> broadcast (Bounds b) model
            | Resource r -> broadcast (Resource r) model
    }