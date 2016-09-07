module Border

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows
open Brush

type Event =
  | Width of float32
  | Brush of Brush.Model

type RenderTargetResource<'a when 'a :> IDisposable> =
  { target: RenderTarget
    resource: 'a
  }
    member x.Dispose() = 
        x.resource.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

