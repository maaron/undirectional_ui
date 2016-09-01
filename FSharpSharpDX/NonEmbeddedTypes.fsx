
#r "../packages/SharpDX.3.0.2/lib/net45/SharpDX.dll"
#r "../packages/SharpDX.DXGI.3.0.2/lib/net45/SharpDX.DXGI.dll"
#r "../packages/SharpDX.Direct2D1.3.0.2/lib/net45/SharpDX.Direct2D1.dll"
#r "../packages/SharpDX.Mathematics.3.0.2/lib/net45/SharpDX.Mathematics.dll"
#r "../packages/SharpDX.Desktop.3.0.2/lib/net45/SharpDX.Desktop.dll"
#r "../packages/SharpDX.Direct3D11.3.0.2/lib/net45/SharpDX.Direct3D11.dll"
#r "System.Runtime.Extensions"
#r "System.Windows.Forms"

open System
open SharpDX
open SharpDX.Direct2D1
open SharpDX.Direct3D
open SharpDX.Mathematics
open SharpDX.Mathematics.Interop
open SharpDX.Windows

let asdfwqer = [1;2;3]

type Render = RenderTarget -> unit

type Cmd<'e> = 'e list * ((unit -> unit) -> unit)

module Cmd =
    let none: Cmd<'a> = 
        ([], ignore)

    let map (f: 'a -> 'b) (cmd: Cmd<'a>): Cmd<'b> = 
        (List.map f (fst cmd), snd cmd)

    let batch (cmds: Cmd<'a> list): Cmd<'a> = 
        let events = [for cmd in cmds do 
                      for a in (fst cmd) do 
                      yield a]
        let start callback = for c in cmds do snd c callback
        (events, start)

type Component<'e, 'm> = {
    init: 'm * Cmd<'e>
    view: 'm -> Render
    update: 'e -> 'm -> 'm * Cmd<'e>
}

type ResourceEvent =
    | Create of RenderTarget
    | Release

type Resource<'m> = Component<ResourceEvent, 'm>

type ContentEvent =
    | Bounds of Size2F
    | Resource of ResourceEvent

type ContentModel<'m> = {
    bounds: Size2F
    content: 'm
}

type Content<'m> = Component<ContentEvent, ContentModel<'m>>

type InputEvent = 
    | Mouse of Vector2
    | LeftButton of bool
    | RightButton of bool

type InterfaceEvent<'e> =
    | Input of InputEvent
    | Content of ContentEvent
    | Event of 'e

type Interface<'e, 'm, 'c> = Component<InterfaceEvent<'e>, ContentModel<'m>>

type InterfaceUpdate<'e, 'm> = InterfaceEvent<'e> -> ContentModel<'m> -> ContentModel<'m> * Cmd<InterfaceEvent<'e>>

type Layout = {
    size: Size2F
    transform: Matrix3x2
    inverse: Matrix3x2
}

type Arrange = Size2F -> Size2F -> Layout

type ArrangedModel<'m> = {
    layout: Layout
    content: ContentModel<'m>
}

let arranged (arrange: Arrange) (ui: Interface<'e, 'm, 'c>): Interface<'e, ArrangedModel<'m>> = 
    { 
        init = 
            let (sub, cmd) = ui.init
            let model =
              { bounds = Size2F.Zero
                content = 
                  { content = sub
                    layout = 
                      { size = Size2F.Zero
                        transform = Matrix3x2.Identity
                        inverse = Matrix3x2.Identity } } }
            (model, cmd) 

        view =
            fun m t ->
                let old = t.Transform
                let layout = (arrange m.bounds m.content.content.bounds)
                t.Transform <- Matrix3x2.op_Implicit(layout.transform)
                ui.view m.content.content t
                t.Transform <- old

        update =
            fun e m -> 
                match e with
                | Input (Mouse p) -> 
                    let pmapped = Matrix3x2.TransformPoint(m.content.layout.inverse, p)
                    let (newContent, cmd) = ui.update (Input (Mouse pmapped)) m.content.content
                    ({ m with content = { m.content with content = newContent } }, cmd)
                
                | Content (Bounds s) -> 
                    let layout = arrange s m.content.content.bounds
                    let (newContent, cmd) = ui.update (Content (Bounds layout.size)) m.content.content
                    ({ bounds = s; content = { content = newContent; layout = layout } }, cmd)
                
                | _ -> 
                    let (newContent, cmd) = ui.update e m.content.content
                    ({ m with content = { m.content with content = newContent } }, cmd)
    }

type RectangleEvent =
    | Size of Size2F
    | Color of Color

type RectangleModel = {
    color: Color
    brush: SolidColorBrush option
    strokeWidth: float32
    strokeStyle: StrokeStyle option
}

let rectangle initialSize initialColor: Interface<RectangleEvent, RectangleModel> =
  { init =
        let model = 
          { bounds = initialSize
            content = 
              { color = initialColor
                brush = None 
                strokeWidth = 1.0f 
                strokeStyle = None } }
        let cmd = Cmd.none
        (model, cmd)

    view = 
        fun m t -> 
            match m.content.brush with
            | Some brush -> 
                match m.content.strokeStyle with
                | Some style ->
                    t.DrawRectangle(
                        RawRectangleF(
                            0.0f, 0.0f, 
                            m.bounds.Width, 
                            m.bounds.Height), 
                        brush, 
                        m.content.strokeWidth,
                        style)
                | None ->
                    t.DrawRectangle(
                        RawRectangleF(
                            0.0f, 0.0f, 
                            m.bounds.Width, 
                            m.bounds.Height), 
                        brush, 
                        m.content.strokeWidth)
            | None -> ()

    update =
        fun e m ->
            match e with
            | Event (Size s) -> ({ m with bounds = s }, Cmd.none)
            | Event (Color c) ->
                if m.content.brush.IsSome then m.content.brush.Value.Color <- Color.op_Implicit(c)
                ({ m with content = { m.content with color = c } }, Cmd.none)
            
            | Content (Resource (Create t)) -> 
                let brush = Some (new SolidColorBrush(t, Color.op_Implicit(m.content.color)))
                ({ m with content = { m.content with brush = brush } }, Cmd.none)
            
            | Content (Resource (Release)) ->
                match m.content.brush with | Some b -> b.Dispose() | None -> ()
                match m.content.strokeStyle with | Some s -> s.Dispose() | None -> ()
                ({ m with content = { m.content with brush = None; strokeStyle = None }}, Cmd.none)
            
            | _ -> (m, Cmd.none)
  }

let augment (init: 'm2) (update: InterfaceUpdate<'e, 'm> -> InterfaceEvent<'e> -> ('m2 * ContentModel<'m>) -> ('m2 * ContentModel<'m>) * Cmd<InterfaceEvent<'e>>) (ui: Interface<'e, 'm, 'c>): Interface<'e, 'm2 * 'm> =
  { init = 
        let (sub, cmd) = ui.init
        let model = { bounds = sub.bounds; content = (init, sub.content) }
        (model, cmd)
    
    view = fun m -> ui.view { bounds = m.bounds; content = snd m.content }
    
    update = 
        fun e outer -> 
            let (b, content) = outer.content
            let m = { bounds = outer.bounds; content = content }
            let ((bnew, contentnew), cmd) = update ui.update e (b, m)
            ({ bounds = contentnew.bounds; content = (bnew, contentnew.content) }, cmd)
  }

// This is an example of a combinator that generates new events that only travel forward within 
// the local context.  This is in contrast to commands, which indirectly cause events to arrive 
// at the application root.  This is useful in cases where we don't need to be aware of the event
// outside of the context of the combinator, i.e., combinators upstream from this one won't 
// receive the event.
let onmouseover handler ui =
    augment false 
        (fun update e (b, m) -> 
            match e with
            | Input (Mouse mouse) -> 
                let isOver = mouse.X >= 0.0f && mouse.X <= m.bounds.Width 
                            && mouse.Y >= 0.0f && mouse.Y <= m.bounds.Height
                let (normalUpdate, cmd) = ui.update e m
                let (mouseUpdate, cmds) = 
                    if not isOver = b then 
                        let (sub, cmd2) = ui.update (Event (handler isOver)) normalUpdate
                        (sub, Cmd.batch [cmd; cmd2])
                    else (normalUpdate, cmd)
                ((isOver, normalUpdate), cmds)
            | _ -> 
                let (updated, cmd) = ui.update e m
                ((b, updated), cmd)
        )

let mapBool yes no b = if b then yes else no

let app = onmouseover (mapBool (Color Color.Blue) (Color Color.Green)) (rectangle (Size2F(10.0f, 20.0f)) Color.Green)


let form = new RenderForm("Test")
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

do  RenderLoop.Run(form, fun () -> 
                            d2dContext.BeginDraw()
                            d2dContext.Clear(Nullable<RawColor4>(Color4.op_Implicit(Color4.Black)))
                            d2dContext.FillEllipse(new Ellipse(RawVector2(300.0f,300.0f), 50.0f, 50.0f), brush)
                            d2dContext.EndDraw()
                            swapChain.Present(0,DXGI.PresentFlags.None)
                            )
0