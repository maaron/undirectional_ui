
module Virtualized

open SharpDX
open Arranged
open Translated
open Draw.Drawing
open Geometry
open Ui
open Cmd

type Event<'e, 'p> =
  | Items of 'p list
  | Item of int * 'e

type RemainderArrangement = {
    arrangement: Arrangement
    remainder: Arrangement
}

type RemainderArranger = Point -> Point -> RemainderArrangement

type Generator<'t, 'e, 'm> = 't -> Ui<'e, 'm>

type ItemArrangement<'e, 'm> = {
    model: Arranged.Model<'m>
    ui: Ui<'e, 'm>
    arranged: Ui<'e, Arranged.Model<'m>>
    drawing: Drawing
}

type ItemContainer<'e, 't, 'm> = {
    item: 't
    arrangement: ItemArrangement<'e, 'm> option
}

type Model<'e, 't, 'm> = {
    available: Point
    size: Point
    items: ItemContainer<'e, 't, 'm> list
}

let virtualized (arranger: RemainderArranger) (generate: Generator<'t, 'e, 'm>) =
    {
    init = 
        (
            {
            available = Point.zero
            size = Point.zero
            items = []
            },
            Cmd.none
        )

    view = 
        fun model ->
            {
                size = model.size
                clip = None
                transform = Matrix3x2.Identity
                commands = model.items |> List.map (fun item -> item.arrangement.drawing)
            }

    update =
        // Implementation Summary:
        // Bounds -> re-draw
        // Input -> broadcast, re-draw items following any item whose drawize size changes
        // Event (Items items) -> re-generate, re-draw
        // Event (Item i) -> skip items before i, update and re-draw items[i], re-draw rest if items[i] drawing size changes

        let redraw model =
            let initialState = 
                {
                size = model.available
                clip = None
                transform = Matrix3x2.Identity
                inverse = Matrix3x2.Identity
                }

            let mapFolder (remaining, size, cmd) itemContainer =
                // If we haven't generated the UI yet, generate a model.
                // Do the same thing as the arranged combinator, but apply the remaining 
                // arrangement and save the new remaining arrangement for the next iteration.
                // 
                ({ itemContainer with arrangement = arrangement }, newState)

            let (items, (finalState, finalSize)) = 
                model.items |> List.mapFold mapFolder (initialState, Point.zero, Cmd.none)

            { model with items = items; size = finalSize }

        let updateBounds model bounds =
            model.items |> List.mapFold
                (fun remaining (itemData, itemUi) -> 
                    match itemUi with
                    | Some (itemModel, itemDrawing) -> 
                        let (newItemModel, itemCmd) = translated remaining.topLeft
                    (item, remaining)
                )
                (Rectangle.fromPoints Point.zero bounds)

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
            | Event (Items items) -> updateItems model items
            | Event (Item (index, e)) -> updateItem model index (Event e)
            | Input i -> broadcast (Input i) model
            | Bounds b -> updateBounds model b
    }