module Draw.Drawing

open System
open SharpDX
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows
open Geometry
open Draw.Primitive

type Drawing = {
    transform: Matrix3x2
    clip: Rectangle
    commands: Command list
}
and Command =
    | RectangleFill of RectangleFill
    | RectangleStroke of RectangleStroke
    | Drawing of Drawing

let rec sizeof drawing =
    let sizeofCommand transform clip accum command =
        match command with
        | RectangleFill r -> 
            let bottomRight = (Rectangle.transformBounds transform r.geometry).bottomRight
            Point.maxCoords (Point.minCoords bottomRight clip.bottomRight) accum
        
        | RectangleStroke r -> 
            let bottomRightMidStroke = (Rectangle.transformBounds transform r.geometry).bottomRight
            let halfWidth = r.stroke.width / 2.0f
            let bottomRight = { x = bottomRightMidStroke.x + halfWidth; y = bottomRightMidStroke.y + halfWidth }
            Point.maxCoords (Point.minCoords bottomRight clip.bottomRight) accum

        | Drawing d -> sizeof d

    drawing.commands |> List.fold (sizeofCommand drawing.transform drawing.clip) Point.zero