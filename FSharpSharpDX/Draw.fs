module Draw.Drawing

open System
open System.Collections.Generic
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows
open Geometry
open Draw.Primitive

type Drawing = {
    size: Point
    clip: Rectangle option
    transform: Matrix3x2
    commands: Command list
}
and Command =
    | Rectangle of RectangleFill
    | RectangleStroke of RectangleStroke
    | Drawing of Drawing

type Resource =
    | Brush of Brush

type ResourceCache() =
    let mutable solidColorBrush = None
    
    let linearGradientBrushes = Dictionary<float32 * Matrix3x2, SharpDX.Direct2D1.LinearGradientBrush>()

    let gradientStopCollections = Dictionary<GradientStop list, SharpDX.Direct2D1.GradientStopCollection>()

    member this.get target resource =
        match resource with
        | Brush (Solid color) ->
            solidColorBrush <- 
                match solidColorBrush with 
                | Some s -> Some s 
                | None -> Some (new SolidColorBrush(target, Color.op_Implicit(color)))
            solidColorBrush.Value.Color <- color

        | Brush (Linear linear) ->
            let gradientStops =
                match gradientStopCollections.

            let brush =
                match linearGradientBrushes.TryGetValue((linear.opacity, linear.transform)) with
                | (true, value) -> value
                | (false, _) -> 
                    linearGradientBrushes.Add(
                        new LinearGradientBrush(
                            target,
                            LinearGradientBrushProperties(StartPoint = linear.start, EndPoint = linear.stop),
                            BrushProperties(Opacity = linear.opacity, Transform = linear.transform),
                            GradientStopCollection(target, List.toArray linear.stops)))

type Target(native: RenderTarget)

let rec render target drawing =
    let originalTransform = target.Transform
    target.Transform <- Matrix3x2.op_Implicit(drawing.transform)
    drawing.clip |> Option.map (fun clip -> target.PushAxisAlignedClip(RawRectangleF(clip.topLeft.x, clip.topLeft.y, clip.bottomRight.x, clip.bottomRight.y), AntialiasMode.Aliased)) |> ignore
    for cmd in drawing.commands do
        match cmd with
        | Rectangle r -> 
    drawing.clip |> Option.map (fun clip -> target.PopAxisAlignedClip()) |> ignore