// Learn more about F# at http://fsharp.org
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
open Overlayed

type MainForm<'e, 'm>(ui: Interface<'e, 'm>) =
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
            this.update (Content (Resource (Create rt)))
            resourcesAllocated <- true

        rt.BeginDraw()
        rt.Clear(Nullable<RawColor4>(Color.op_Implicit(Color.Black)))
        view rt
        
        try
            rt.EndDraw()
        with
        | _ -> 
            this.update (Content (Resource Release))
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
        this.update (Content (Bounds sizef))
        ()

    override this.OnLoad(e: EventArgs) =
        let size = Size2(this.ClientSize.Width, this.ClientSize.Height)
        let sizef = Size2F(float32(this.ClientSize.Width), float32(this.ClientSize.Height))
        rt.Resize(Size2(this.ClientSize.Width, this.ClientSize.Height))
        this.update (Content (Bounds sizef))
        ()

    override this.OnMouseMove(e: MouseEventArgs) =
        this.update (Input (MouseMove (Vector2(float32(e.X), float32(e.Y)))))

    override this.OnMouseLeave(e: EventArgs) =
        this.update (Input MouseLeave)

    override this.OnClosed(e: EventArgs) =
        this.update (Content (Resource Release))
        if not (rt = null) then rt.Dispose()
        factory.Dispose()

let makeForm (ui: Interface<'e, 'm>) = new MainForm<'e, 'm>(ui)

[<EntryPoint>]
let main argv = 
    // The application
    let mapBool yes no b = if b then yes else no
    
    let box = 
        rectangle [
            Fill (Solid Color.Green)
            Size (Size2F(200.0f, 200.0f)) ]
     |> padded 10.0f
     |> onmouseover (mapBool (Fill (Solid Color.Blue)) (Fill (Solid Color.Green)))

    let innerBox =
        rectangle [
            Fill (Solid Color.Red)
            Rectangle.Size (Size2F(100.0f, 200.0f)) ]
     |> margined 10.0f

    let app = 
        (overlayed box innerBox)
     |> margined 10.0f

    let form = makeForm app

    // Modern style D2D usage?
    #if false
    let d3device = new Direct3D11.Device(Direct3D.DriverType.Hardware,  Direct3D11.DeviceCreationFlags.BgraSupport ||| Direct3D11.DeviceCreationFlags.Debug) //not sure if or flags are working correctly
    let defDevice = d3device.QueryInterface<Direct3D11.Device1>()
    let dxgiDevice2 = defDevice.QueryInterface<DXGI.Device2>()
    let dxgiAdapter = dxgiDevice2.Adapter
    let dxgiFactory2 = dxgiAdapter.GetParent<DXGI.Factory2>()
    let scDescription = ref (new DXGI.SwapChainDescription1(
                                    Width = 0,
                                    Height = 0,
                                    Format = DXGI.Format.B8G8R8A8_UNorm,
                                    Stereo = RawBool(false),
                                    SampleDescription = new DXGI.SampleDescription(1,0),
                                    Usage = DXGI.Usage.RenderTargetOutput,
                                    BufferCount = 2,
                                    Scaling = DXGI.Scaling.None,
                                    SwapEffect = DXGI.SwapEffect.FlipSequential
                                    ))

    let swapChain = new DXGI.SwapChain1(dxgiFactory2, defDevice, form.Handle, scDescription, System.Nullable(), null)
    let d2dDevice = new Direct2D1.Device(dxgiDevice2)
    let d2dContext = new Direct2D1.DeviceContext(d2dDevice, Direct2D1.DeviceContextOptions.None)
    let fac = new Direct2D1.Factory(FactoryType.SingleThreaded)
    let dpi = fac.DesktopDpi
    let bMProperties = new BitmapProperties1(new PixelFormat(DXGI.Format.B8G8R8A8_UNorm, AlphaMode.Premultiplied), dpi.Height, dpi.Width, BitmapOptions.CannotDraw ||| BitmapOptions.Target)
    let bb = swapChain.GetBackBuffer<DXGI.Surface>(0)
    let target = new Bitmap1(d2dContext,bb, bMProperties)
    do  d2dContext.Target <- target
    let brush = new SolidColorBrush(d2dContext, Color4.op_Implicit(Color4.White))
    #endif

    Application.Run(form);
    
    // Game-style render loop
    #if false
    do  RenderLoop.Run
            (
                form, 
                fun () -> 
                    d2dContext.BeginDraw()
                    d2dContext.Clear(Nullable<RawColor4>(Color4.op_Implicit(Color4.Black)))
                    d2dContext.FillEllipse(new Ellipse(RawVector2(300.0f,300.0f), 50.0f, 50.0f), brush)
                    d2dContext.EndDraw()
                    swapChain.Present(0,DXGI.PresentFlags.None)
            )
    #endif
    0
