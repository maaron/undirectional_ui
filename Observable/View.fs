module View

open Geometry
open Drawing

type View =
  { size: Point
    drawing: Drawing }

    with
        static member pad width { size = size; drawing = drawing } = 
          { size = { x = size.x + width * 2.0; y = size.y + width * 2.0 }
            drawing = DrawTransformed (Matrix3x2.Translation { x = width; y = width }, drawing) }

        static member margin newsize width { size = size; drawing = drawing } = 
          { size = newsize
            drawing = DrawTransformed (Matrix3x2.Translation (Point.square width), drawing) }

        static member overlay bottom top =
          { size = Point.max bottom.size top.size
            drawing = DrawGroup [bottom.drawing; top.drawing] }

let view size drawing = { size = size; drawing = drawing }