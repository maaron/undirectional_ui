
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

type RemainderLayout = {
    limit: Point -> Point
    arrangeRemainder: Point -> Point -> RemainderArrangement
}

type Generator<'t, 'e, 'm> = 't -> Ui<'e, 'm>

type ItemArrangement<'e, 'm> = {
    model: 'm
    ui: Ui<'e, 'm>
    arrangement: RemainderArrangement
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

let layoutFromRemainder remainderLayout =
    {
    limit = remainderLayout.limit
    arrange = (fun available desired -> (remainderLayout.arrangeRemainder available desired).arrangement)
    }

let virtualized (layout: RemainderLayout) (generate: Generator<'t, 'e, 'm>) =
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
                commands = 
                    model.items 
                    |> List.choose (fun item -> item.arrangement)
                    |> List.map (fun a -> Drawing a.drawing)
            }

    update =
        // Implementation Summary:
        // Bounds -> re-draw
        // Input -> broadcast, re-draw items following any item whose drawize size changes
        // Event (Items items) -> re-generate, re-draw
        // Event (Item i) -> skip items before i, update and re-draw items[i], re-draw rest if items[i] drawing size changes

        let mapCmd index cmd = Cmd.map (fun e -> Item (index, e)) cmd

        let redrawItem (remaining: Rectangle) itemContainer =
            match itemContainer.arrangement with
            | Some currentArrangement -> (itemContainer, Cmd.none)
            | None ->
                let itemUi = generate itemContainer.item 
                let (uiModel, initCmd) = itemUi.init
                let itemDrawing = itemUi.view uiModel
                let limit = layout.limit remaining.size
                let itemArrangement = 
                    Some 
                        {
                        model = uiModel
                        ui = itemUi
                        arrangement = remainderArrangement
                        drawing = itemDrawing
                        }
                ({ itemContainer with arrangement = itemArrangement }, initCmd)
        
        let redraw model =
            let initialState = 
                {
                size = model.available
                clip = None
                transform = Matrix3x2.Identity
                inverse = Matrix3x2.Identity
                }

            let mapFolder (remaining: Arrangement, bounds, cmd) itemContainer =
                if not (Point.isEmpty remaining.size) then 
                    (itemContainer, (remaining, bounds, cmd))
                else
                    match itemContainer.arrangement with
                    | Some currentArrangement -> (itemContainer, (remaining, bounds, cmd))
                    | None ->
                        let itemUi = generate itemContainer.item 
                        let (uiModel, initCmd) = itemUi.init
                        let itemDrawing = itemUi.view uiModel
                        let remainderArrangement = arranger remaining.size itemDrawing.size
                        let arrangement =
                            {
                            model = uiModel
                            ui = itemUi
                            arrangement = remainderArrangement
                            drawing = itemDrawing
                            }
                        let newRemaining = remainderArrangement.remainder
                        let newBounds = 
                            let itemBounds = 
                                Rectangle.fromPoints Point.zero remainderArrangement.arrangement.size
                                |> Rectangle.transformBounds remainderArrangement.arrangement.transform
                            if bounds = Rectangle.zero then itemBounds
                            else Rectangle.union bounds itemBounds
                        ({ itemContainer with arrangement = Some arrangement }, (newRemaining, newBounds, Cmd.batch [cmd; initCmd]))

            let (items, (finalState, finalSize, cmd)) = 
                model.items |> List.mapFold mapFolder (initialState, Rectangle.zero, Cmd.none)

            ({ model with items = items; size = finalSize.size }, cmd)

        fun event model ->
            match event with
            //| Event (Items items) -> updateItems model items
            //| Event (Item (index, e)) -> updateItem model index (Event e)
            //| Input i -> broadcast (Input i) model
            | Bounds b -> redraw { model with available = b }
    }