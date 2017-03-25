﻿module Direct2D1

open System.Collections.Generic
open Drawing

open SharpDX
open SharpDX.Direct2D1

type ResourceCache() =
    let mutable solidColorBrush = None
    
    let linearGradientBrushes = Dictionary<float32 * Matrix3x2, SharpDX.Direct2D1.LinearGradientBrush>()

    let gradientStopCollections = Dictionary<GradientStop list, SharpDX.Direct2D1.GradientStopCollection>()

    let strokeStyles = Dictionary<StrokeStyle, SharpDX.Direct2D1.StrokeStyle>()

    member this.getBrush target resource =
        match resource with
        | Solid color ->
            solidColorBrush <- 
                match solidColorBrush with 
                | Some s -> Some s 
                | None -> Some (new SolidColorBrush(target, Color.op_Implicit(color)))
            solidColorBrush.Value.Color <- Color.op_Implicit(color)
            solidColorBrush.Value :> SharpDX.Direct2D1.Brush

        | Linear linear ->
            let gradientStops =
                match gradientStopCollections.TryGetValue(linear.stops) with
                | (true, value) -> value
                | (false, _) ->
                    let stops =
                        new GradientStopCollection(
                                target, List.toArray linear.stops)
                    gradientStopCollections.Add(linear.stops, stops)
                    stops
        
            let brush =
                match linearGradientBrushes.TryGetValue((linear.opacity, linear.transform)) with
                | (true, value) -> value
                | (false, _) -> 
                    let brush =
                        new LinearGradientBrush(
                            target,
                            LinearGradientBrushProperties(
                                StartPoint = RawVector2(linear.start.x, linear.start.y),
                                EndPoint = RawVector2(linear.stop.x, linear.stop.y)),
                            System.Nullable(
                                BrushProperties(
                                    Opacity = linear.opacity,
                                    Transform = Matrix3x2.op_Implicit(linear.transform))),
                            gradientStops)
                    linearGradientBrushes.Add((linear.opacity, linear.transform), brush)
                    brush

            brush.StartPoint <- RawVector2(linear.start.x, linear.start.y)
            brush.EndPoint <- RawVector2(linear.stop.x, linear.stop.y)
            brush :> SharpDX.Direct2D1.Brush

        member this.getStrokeStyle (factory: Factory) resource =
            match strokeStyles.TryGetValue(resource) with
            | (true, s) -> s
            | (false, _) ->
                let s = new SharpDX.Direct2D1.StrokeStyle(factory, resource.properties, List.toArray resource.dashes)
                strokeStyles.Add(resource, s)
                s

        member this.ReleaseTargetResources() =
            solidColorBrush <-
                match solidColorBrush with
                | Some b -> b.Dispose(); None
                | None -> None

            for linear in linearGradientBrushes.Values do linear.Dispose()
            linearGradientBrushes.Clear()

            for grad in gradientStopCollections.Values do grad.Dispose()
            gradientStopCollections.Clear()

        member this.ReleaseFactoryResources() =
            for s in strokeStyles.Values do s.Dispose()
            strokeStyles.Clear()

let rec renderCommand (target: RenderTarget) (cache: ResourceCache) command =
    match command with
    | RectangleFill r -> 
        target.FillRectangle(
            RawRectangleF(
                r.geometry.topLeft.x,
                r.geometry.topLeft.y,
                r.geometry.bottomRight.x,
                r.geometry.bottomRight.y),
            cache.getBrush target r.brush)

    | RectangleStroke r -> 
        target.DrawRectangle(
            RawRectangleF(
                r.geometry.topLeft.x,
                r.geometry.topLeft.y,
                r.geometry.bottomRight.x,
                r.geometry.bottomRight.y),
            cache.getBrush target r.stroke.brush,
            r.stroke.width,
            match r.stroke.style with
            | Some s -> cache.getStrokeStyle target.Factory s
            | None -> null)

    | Clipped (rect, d) ->
        target.PushAxisAlignedClip(
            RawRectangleF(
                rect.topLeft.x, rect.topLeft.y, 
                rect.bottomRight.x, rect.bottomRight.y),
            AntialiasMode.Aliased)

        renderCommand target cache d

        target.PopAxisAlignedClip()

    | Transformed (matrix, d) ->
        let originalTransform = target.Transform

        target.Transform <- 
            Matrix3x2.op_Implicit(
                Matrix3x2.Multiply(
                    Matrix3x2.op_Implicit(originalTransform), 
                    matrix))

        renderCommand target cache d

        target.Transform <- originalTransform

    | Drawings cmds ->
        cmds |> List.iter (renderCommand target cache)

let render target cache drawing = renderCommand target cache drawing.drawing


