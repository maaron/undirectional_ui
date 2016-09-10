
module Stacked

open SharpDX
open Translated
open Ui

let scanMap (folder: 's -> 't -> 'r * 's) (state: 's) (source: seq<'t>): seq<'r * 's> =
    let mutable ms = state;
    seq { for item in source do 
          let (r, s) = folder ms item
          ms <- s
          yield (r, s) }

type Event<'e, 'p> =
  | List of 'p list
  | Item of int * 'e

type Model<'e, 'm> = Size2F * (Ui<'e, Arranged.Model<'m>> * Arranged.Model<'m>) list

let stacked (template: 'p -> Ui<'e, 'm>) =

    let generateItems = Seq.map template

    let reduce (remaining: RectangleF) (used: Size2F) =
                RectangleF(
                    remaining.X, 
                    remaining.Y + used.Height, 
                    remaining.Width, 
                    remaining.Height - used.Height)
            
    let mapCmd index cmd = Cmd.map (fun e -> Item (index, e)) cmd

    {
    init = ((Size2F.Zero, []), Cmd.none)

    bounds = fst
        #if false
            model 
         |> List.fold 
                (fun accum (ui, item) -> 
                    let itemSize = ui.bounds item
                    Size2F(
                        max accum.Width itemSize.Width,
                        accum.Height + itemSize.Height)
                )
                (Size2F(0.0f, 0.0f))
        #endif

    view = 
        let renderItem target (ui, model) = ui.view model target
        fun (size, items) target -> items |> List.iter (renderItem target)

    update =
        // TODO: This still needs to detect bounds changes in items and update layouts of 
        // subsequent items for all events.  Perhaps all the updates can be factored into a single 
        // function that performs layout.
        fun event model ->
            let (bounds, uis) = model

            let arrangeItems items = 
                let (arrangedItems, (cmd, used)) = 
                    items
                 |> scanMap
                      ( fun (i, used, remaining: RectangleF, cmds) ui -> 
                            let arranged = translated (Vector2(remaining.X, remaining.Y)) ui
                            let (model, cmd) = arranged.init
                            let itemSize = arranged.bounds model
                            ((arranged, model), (i + 1, (Geometry.maxSize used itemSize), reduce remaining itemSize, Cmd.batch [cmds; mapCmd i cmd]))
                      )
                      (0, Size2F.Zero, RectangleF(0.0f, 0.0f, bounds.Width, bounds.Height), Cmd.none)
                      
                 |> Seq.takeWhile (fun (ui, (i, used, remaining, cmd)) -> remaining.Height > 0.0f && remaining.Width > 0.0f)
                 |> Seq.mapFold (fun s (item, (i, used, remaining, cmd)) -> (item, (cmd, used))) (Cmd.none, Size2F.Zero)
                
                ((used, Seq.toList arrangedItems), cmd)

            let updateItem (bounds, items) index e =
                let (updatedItems, cmd) =
                    items
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

                ((bounds, updatedItems), cmd)

            let broadcast e (bounds, items) =
                let (updatedItems, (count, cmd)) =
                    items
                 |> List.mapFold
                      ( fun (i, cmds) (ui, itemModel) -> 
                            let (updated, cmd) = ui.update e itemModel
                            ((ui, updated), (i + 1, Cmd.batch [cmds; mapCmd i cmd])))
                      (0, Cmd.none)
                ((bounds, updatedItems), cmd)

            match event with
            | Event (List items) -> arrangeItems (generateItems items)
            | Event (Item (index, e)) -> updateItem model index (Event e)
            | Input i -> broadcast (Input i) model
            | Bounds b -> broadcast (Bounds b) model
            | Resource r -> broadcast (Resource r) model
    }