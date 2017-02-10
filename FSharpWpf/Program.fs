
open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Shapes
open System.Windows.Media

let style = new Style();
// Triggers, Resources, TargetType, Setters maybe aren't necessary?

// This seems to be necessary to at least have "content" and "children" properties, but what 
// methods must this have?  Another possibility is to encode all controls as union cases, but then 
// it's not "externally" extensible.  Perhaps a method that performs update given a previous
// ControlD value and a "live" Control?

// Many properties are "option" types below, and in some of these cases it means "there is no 
// value", e.g., event handlers.  In other cases, it means "use the default" value.  I'm not sure 
// whether there is a common semantic in WPF-land regarding what a "default" value is.  Some 
// possibilities are:
//  - Button.BackgroundProperty.DefaultMetadata.DefaultValue
//  - control.ClearValue(TextBox.BorderBrushProperty)

module View =
    let processEvent context e = printf "event: %O\n" e

    type Cmd<'Msg> = class end

    // Placeholder for some type that encapsulates things like the event processor and resource 
    // cache.
    type Context () = 
        member this.update msg = ()

    type Brush =
        | Solid of Color

        member view.create context =
            match view with
            | Solid c -> SolidColorBrush(c)

    type Font =
      { family: FontFamily option
        size: float
        stretch: FontStretch option
        style: FontStyle option
        weight: FontWeight option }

    type Control<'Msg> = 
        abstract member create: Context -> obj
        //abstract member update: obj -> Context -> obj

    type Empty<'Msg> = | Empty
        with 
            interface Control<'Msg> with
                member view.create context = null
                //member view.update view' context = null

    type Rectangle<'Msg> =
      { width: float
        height: float
        stroke: Brush option
        fill: Brush option }
        interface Control<'Msg> with
            member view.create context =
                let control = Rectangle(Width = view.width, Height = view.height)
                view.stroke |> Option.iter (fun x -> control.Stroke <- x.create context)
                view.fill |> Option.iter (fun x -> control.Fill <- x.create context)
                control :> obj

    type Button<'Msg> =
      { width: float
        height: float
        content: Control<'Msg>
        onclick: (RoutedEventArgs -> 'Msg) option }
        
        interface Control<'Msg> with
            member view.create context =
                let control = 
                    Button( 
                        Width = view.width, 
                        Height = view.height, 
                        Content = view.content.create context)

                view.onclick |> Option.iter (fun tagger -> 
                    control.Click.Add (fun e -> 
                        processEvent context (tagger e) ))
                control :> obj

    type TextBlock<'Msg> =
      { text: string
        foreground: Brush option
        background: Brush option
        font: Font option }

        interface Control<'Msg> with
            member view.create context =
                let control = TextBlock(Text = view.text)

                view.font |> Option.iter (fun font -> 
                    control.FontSize <- font.size
                    font.family |> Option.iter (fun x -> control.FontFamily <- x)
                    font.stretch |> Option.iter (fun x -> control.FontStretch <- x)
                    font.style |> Option.iter (fun x -> control.FontStyle <- x)
                    font.weight |> Option.iter (fun x -> control.FontWeight <- x))

                // This generates a brush object every time it is used, but we could try to reuse.
                view.foreground |> Option.iter (fun x -> control.Foreground <- x.create context)
                view.background |> Option.iter (fun x -> control.Background <- x.create context)

                control :> obj

    type Window<'Msg> =
      { width: float
        height: float
        content: Control<'Msg> }

        interface Control<'Msg> with
            member view.create context =
                let w = 
                    Window
                      ( Width = view.width, 
                        Height = view.height,
                        Content = view.content.create context )

                w :> obj

            
    type ui =
        static member window (?width, ?height, ?content) =
          { width = defaultArg width nan
            height = defaultArg height nan
            content = defaultArg content (Empty :> Control<_>) }

        static member button (?width, ?height, ?onclick, ?content) =
          { width = defaultArg width nan
            height = defaultArg height nan
            onclick = onclick
            content = defaultArg content (Empty :> Control<_>) }

        static member rectangle (width, height, stroke, fill) =
          { width = width
            height = height
            stroke = stroke 
            fill = fill }

        static member text (text, ?font, ?foreground, ?background) =
          { text = text
            font = font
            foreground = foreground
            background = background }

    let run init update view =
        let context = Context()
        let app = new Application()
        let w = (window :> Control<_>).create context
        app.Run((w :?> Window))

    type App<'Msg, 'Model> (init: 'Model, update: 'Msg -> 'Model -> ('Model * Cmd<'Msg>), view: ('Model -> Control<'Msg>)) =
        

open View

[<EntryPoint>]
[<STAThread>]
let main argv = 
    
    let window = 
        ui.window (
            width = 200.0, height = 200.0,
            content = ui.button (
                onclick = (fun _ -> 123),
                content = ui.text ("button") 
                )
            )

    run window