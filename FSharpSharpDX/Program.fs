﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Windows.Forms
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

open Ui
open Brush
open Rectangle
open Arranged
open Padded
open Overlayed
open Mouseover
open Bordered
open Stacked
open Virtualized

type MainForm<'e, 'm>(ui: Ui<'e, 'm>) =
    inherit Form()

    let factory = new Direct2D1.Factory(FactoryType.SingleThreaded, DebugLevel.Information)
    let mutable rt: WindowRenderTarget = null

    let (model, cmd) = ui.init
    let mutable state = model
    let mutable view = ui.view model
    let mutable resourcesAllocated = false

    member this.update event =
        let (newState, cmd) = ui.update event state
        state <- newState
        view <- ui.view state
        this.Invalidate()

    override this.OnPaintBackground(e: PaintEventArgs) =
        ()

    override this.OnPaint(e: PaintEventArgs) =
        if not resourcesAllocated then 
            this.update (Resource (Create rt))
            resourcesAllocated <- true

        rt.BeginDraw()
        rt.Clear(Nullable<RawColor4>(Color.op_Implicit(Color.Black)))
        view rt
        
        try
            rt.EndDraw()
        with
        | _ -> 
            this.update (Resource Release)
            resourcesAllocated <- false

    override this.OnHandleCreated(e: EventArgs) =
        base.OnHandleCreated(e)

        let properties = 
            RenderTargetProperties(
                PixelFormat(
                    DXGI.Format.Unknown, 
                    AlphaMode.Premultiplied))

        let hwndProperties = 
            HwndRenderTargetProperties(
                Hwnd = this.Handle, 
                PixelSize = 
                    Size2(
                        this.ClientSize.Width, 
                        this.ClientSize.Height))
        ()
        rt <- new WindowRenderTarget(factory, properties, hwndProperties)

    override this.OnSizeChanged(e: EventArgs) =
        let size = Size2(this.ClientSize.Width, this.ClientSize.Height)
        let sizef = Size2F(float32(this.ClientSize.Width), float32(this.ClientSize.Height))
        rt.Resize(Size2(this.ClientSize.Width, this.ClientSize.Height))
        this.update (Bounds sizef)
        ()

    override this.OnLoad(e: EventArgs) =
        let size = Size2(this.ClientSize.Width, this.ClientSize.Height)
        let sizef = Size2F(float32(this.ClientSize.Width), float32(this.ClientSize.Height))
        rt.Resize(Size2(this.ClientSize.Width, this.ClientSize.Height))
        this.update (Bounds sizef)
        ()

    override this.OnMouseMove(e: MouseEventArgs) =
        this.update (Input (MouseMove (Vector2(float32(e.X), float32(e.Y)))))

    override this.OnMouseLeave(e: EventArgs) =
        this.update (Input MouseLeave)

    override this.OnClosed(e: EventArgs) =
        this.update (Resource Release)
        if not (rt = null) then rt.Dispose()
        factory.Dispose()

let makeForm (ui: Ui<'e, 'm>) = new MainForm<'e, 'm>(ui)

[<EntryPoint>]
let main argv = 
    // The application
    let mapBool yes no b = if b then yes else no
    
    let greenBox = 
        rectangle 
          [ Fill (Solid Color.Green)
            Size (Size2F(200.0f, 200.0f)) ]

    let mouseoverGreenBox =
        greenBox
     |> onmouseover 
          (mapBool (sendEvents [Fill (Solid Color.Blue)])
                   (sendEvents [Fill (Solid Color.Green)]))
     |> bordered 
          [ Stroke.Event.Width 5.0f
            Stroke.Event.Brush (Solid Color.Yellow)
          ]

    let innerBox =
        rectangle 
          [ Fill (Solid Color.Red)
            Size (Size2F(60.0f, 130.0f)) ]
     |> bordered [ Stroke.Event.Width 5.0f; Stroke.Event.Brush (Solid Color.Beige) ]
     |> margined 30.0f

    let template color = 
        rectangle [ Fill (Solid color); Size (Size2F(40.0f, 20.0f)) ]
     |> onmouseover 
          (mapBool (sendEvents [Fill (Solid Color.White)])
                   (sendEvents [Fill (Solid color)]))
     |> bordered [ Stroke.Event.Width 5.0f; Stroke.Event.Brush (Solid Color.Beige) ]
     |> padded 5.0f

    let boxStack = 
        initialize (stackVirtualized template) 
            [ Items [Color.Red; Color.Blue; Color.Green] ]
     |> padded 10.0f

    let colorBordered color ui =
        bordered [ Stroke.Event.Width 5.0f; Stroke.Event.Brush (Solid color) ] ui
    
    let app = 
        mouseoverGreenBox
     |> overlayed boxStack
     //|> overlayed innerBox
     |> padded 10.0f

    let form = 
        makeForm boxStack

    Application.Run(form);
    
    0
