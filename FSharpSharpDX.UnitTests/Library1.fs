namespace FSharpSharpDX.UnitTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open SharpDX
open Draw.Primitive
open Draw.Drawing
open Geometry

[<TestClass>]
type DrawingTests() = 
    
    [<TestMethod>]
    member x.sizeofTests() =
        Assert.IsTrue(
            sizeof 
                { 
                transform = Matrix3x2.Identity
                clip = Rectangle.infinity
                commands = [] 
                } = Point.zero)

        Assert.IsTrue(
            sizeof 
                { 
                    transform = Matrix3x2.Identity
                    clip = Rectangle.infinity
                    commands = 
                    [
                    Command.Rectangle 
                        {
                        geometry = 
                            { 
                            topLeft = Point.zero
                            bottomRight = 
                                {
                                x = 1.0f
                                y = 2.0f
                                }
                            }
                        brush = Solid Color.Transparent
                        }
                    ] 
                } = { x = 1.0f; y = 2.0f })

    [<TestMethod>]
    member x.sizeofTests2() =
        let drawing = 
            { 
            transform = Matrix3x2.Identity
            clip = Rectangle.infinity
            commands = 
                [
                Command.RectangleStroke 
                    {
                    geometry = 
                        { 
                        topLeft = Point.zero
                        bottomRight = 
                            {
                            x = 1.0f
                            y = 2.0f
                            }
                        }
                    stroke = 
                        {
                        brush = Solid Color.Transparent
                        width = 2.0f
                        style = None
                        }
                    }
                ] 
            }

        let expected = { x = 2.0f; y = 3.0f }
        let actual = sizeof drawing
        let test = expected = actual
        
        Assert.IsTrue(test)
