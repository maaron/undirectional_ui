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

open Geometry
open Ui
open Cmd
open Arranged
open Padded
open Overlayed
open Mouseover
open Bordered
open Draw.Primitive
open Rectangle
(*
open Stacked
open Virtualized
*)

type MainForm<'e, 'm>(ui: Ui<'e, 'm>) =
    inherit Form()

    let factory = new Direct2D1.Factory(FactoryType.SingleThreaded, DebugLevel.Information)
    let mutable rt: WindowRenderTarget = null

    let (model, cmd) = ui.init
    let mutable state = model
    let mutable view = ui.view model
    let resourceCache = Draw.Drawing.ResourceCache()

    member this.update event =
        let (newState, cmd) = ui.update event state
        state <- newState
        view <- ui.view state
        this.Invalidate()

    override this.OnPaintBackground(e: PaintEventArgs) =
        ()

    override this.OnPaint(e: PaintEventArgs) =
        (*
        if not resourcesAllocated then 
            this.update (Resource (Create rt))
            resourcesAllocated <- true
        *)

        rt.BeginDraw()
        rt.Clear(Nullable<RawColor4>(Color.op_Implicit(Color.Black)))
        Draw.Drawing.render rt resourceCache view
        
        try
            rt.EndDraw()
        with
        | _ -> 
            resourceCache.ReleaseTargetResources()

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
        let sizef = { x = float32(this.ClientSize.Width); y =  float32(this.ClientSize.Height) }
        rt.Resize(Size2(this.ClientSize.Width, this.ClientSize.Height))
        this.update (Bounds sizef)
        ()

    override this.OnLoad(e: EventArgs) =
        let sizef = { x = float32(this.ClientSize.Width); y =  float32(this.ClientSize.Height) }
        rt.Resize(Size2(this.ClientSize.Width, this.ClientSize.Height))
        this.update (Bounds sizef)
        ()

    override this.OnMouseMove(e: MouseEventArgs) =
        this.update (Input (MouseMove { x = float32(e.X); y = float32(e.Y) }))

    override this.OnMouseLeave(e: EventArgs) =
        this.update (Input MouseLeave)

    override this.OnClosed(e: EventArgs) =
        resourceCache.ReleaseTargetResources()
        resourceCache.ReleaseFactoryResources()
        if not (rt = null) then rt.Dispose()
        factory.Dispose()

let makeForm (ui: Ui<'e, 'm>) = new MainForm<'e, 'm>(ui)

// The application
let mapBool yes no b = if b then yes else no
    
let greenBox = 
    rectangle 
        [ Fill (Solid Color.Green)
          Size { x= 200.0f; y = 200.0f } ]

let mouseoverGreenBox =
    greenBox
    |> onmouseover 
        (mapBool (sendEvents [Fill (Solid Color.Blue)])
                (sendEvents [Fill (Solid Color.Green)]))
    |> bordered 
        [ Width 5.0f
          Brush (Solid Color.Yellow)
        ]

let innerBox =
    rectangle 
        [ Fill (Solid Color.Red)
          Size { x = 60.0f; y = 130.0f } ]
    |> bordered [ Width 5.0f; Brush (Solid Color.Beige) ]
    |> margined 30.0f

let template color = 
    rectangle [ Fill (Solid color); Size { x = 40.0f; y = 20.0f } ]
    |> onmouseover 
        (mapBool (sendEvents [Fill (Solid Color.White)])
                (sendEvents [Fill (Solid color)]))
    |> bordered [ Width 5.0f; Brush (Solid Color.Beige) ]
    |> padded 5.0f
(*
let boxStack = 
    initialize (stackVirtualized template) 
        [ Items [Color.Red; Color.Blue; Color.Green] ]
    |> padded 10.0f
*)
let colorBordered color ui =
    bordered [ Width 5.0f; Brush (Solid color) ] ui
    
let app = 
    rectangle 
        [ Fill (Solid Color.Green)
          Size { x= 200.0f; y = 200.0f } ]
    |> bordered [ Width 5.0f; Brush (Solid Color.Beige) ]

type Foo() =
    member this.UI = app

type FooRecord(ui) =
    member this.Model = ui

let appUi =
    {
    init = (FooRecord(fst app.init), Cmd.none)
    view = fun model -> app.view model.Model
    update = fun event model -> 
        let (uimodel, cmd) = app.update event model.Model
        (FooRecord(uimodel), Cmd.none)
    }

let mappedApp = app |> Mapped.mapEvent (fun (e: unit) -> [])

(*

 - Resource models have side-effects and resource handles that complicate serialization, replay, 
     etc.  It would be nice if we can abstract them away into a separate domain- perhaps part of 
     the RenderTarget.  It maybe isn't a show-stopper, but it means that using "old" models 
     requires the caller to carefully apply Create and Release resource events to ensure they are
     in a usable state and released.  I have to think that a better way is to cache resource 
     handles in a RenderTarget-specific data structure, and use reference-counting to know when 
     resources can be released.  The resources must be collected in this data structure so that 
     they can be released when the RenderTarget says it is necessary.  However, it must be done in
     a way so as not to leak memory (i.e., probably weak references are in order) or require that
     resource descriptors (immutable data structures describing the resource/properties) manage
     resource lifetime explicitly.

 - Encapsulating combinator-generated types in container types is awkward (can only be done in 
     classes, not records).

 - Combinators are thin, and rarely generate commands, but have to handle them correctly.

 - Bounds functions are very often not interesting, but all combinators have to handle them 
     correctly.  Maybe we can put this inside the view member?

*)

[<EntryPoint>]
let main argv = 

    let form = 
        makeForm app

    Application.Run(form);
    
    0
