
module Resource

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

type ResourceState<'resource, 'context> =
  | Created of 'context * 'resource
  | Released

type Bound<'properties, 'resource, 'context> = 'properties * ResourceState<'resource, 'context>

type TargetBound<'properties, 'resource> = Bound<'properties, 'resource, RenderTarget>

type FactoryBound<'properties, 'resource> = Bound<'properties, 'resource, Factory>

let map f bound =
    match snd bound with
    | Created (context, resource) -> f resource; bound
    | _ -> bound