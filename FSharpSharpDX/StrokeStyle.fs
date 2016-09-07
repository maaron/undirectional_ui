
module StrokeStyle

open System
open SharpDX.Direct2D1
open Resource

type Resource = SharpDX.Direct2D1.StrokeStyle

type Properties =
  { properties: StrokeStyleProperties
    dashes: float32 list
  }

type Event = Properties

type Model = FactoryBound<Properties, Resource>

let init = 
  (
    { properties = new StrokeStyleProperties()
      dashes = []
    },
    Released
  )

let release (model: Model) =
    model |> Resource.map (fun r -> r.Dispose())

let create (model: Model) (factory: Factory) =
    let (props, state) = model
    let resource = new StrokeStyle(factory, props.properties, List.toArray props.dashes)
    (fst (release model), Created (factory, resource))

let update event model =
    let (props, state) = model
    match state with
    | Created (factory, resource) -> create (event, state) factory
    | Released -> (event, Released)