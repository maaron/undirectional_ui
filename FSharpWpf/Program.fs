
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

    type Cmd<'Msg> = 'Msg list

    module Cmd =
        let none = []

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
        abstract member update: Control<'Msg> -> obj -> Context -> obj

    type Empty<'Msg> = | Empty
        with 
            interface Control<'Msg> with
                member view.create context = null
                member view.update context view' control = null

    let genericUpdate (update: 'C -> 'C -> 'O -> Context -> 'O): 'C -> Control<'e> -> obj -> Context -> obj when 'C :> Control<'e> =
        fun prev curr' control' context ->
            match control' with
            | :? 'O as control ->
                match curr' with
                | :? 'C as curr -> update prev curr control context :> obj
                | _ -> curr'.create context
            | _ -> failwith "Unexpected WPF control type %s" <| control'.GetType()
    
    type Rectangle<'Msg> =
      { width: float
        height: float
        stroke: Brush option
        fill: Brush option }

        member view.set (control: Rectangle) context =
            control.Width <- view.width
            control.Height <- view.height
            view.stroke |> Option.iter (fun x -> control.Stroke <- x.create context)
            view.fill |> Option.iter (fun x -> control.Fill <- x.create context)
            control

        interface Control<'Msg> with
            member view.create context = view.set (Rectangle()) context :> obj

            member prev.update curr control context =
                let update prev (curr: Rectangle<'Msg>) (control: Rectangle) context = 
                    curr.set control context
                genericUpdate update prev curr control context
                

    type Button<'Msg> =
      { width: float
        height: float
        content: Control<'Msg>
        onclick: (RoutedEventArgs -> 'Msg) option }

        member view.set (control: Button) context =
            control.Width <- view.width
            control.Height <- view.height
            control.Content <- view.content.create context

            // Need some way to retain the IDisposable or RoutedEventHandler instance so that we 
            // can unsubscribe when it comes time to modify the handler or remove it.  Is storing
            // it as a resource or in the context somehow the only way?

            // Also, is it really a good idea to keep around the old view model?  I think WPF 
            // controls already have efficient updates when you modify values (i.e., don't 
            // trigger changes when the value doens't really change).  Maybe it's better to
            // always set normal properties, but still leaves us with what to do about events?
            // Is it ok to always de-register and re-register event handlers every time a new view
            // structure is processed?

            // Perhaps a better way is to just maintain a handler in the context.  We only need to
            // set it once when the control is created.  Whenever we update the tagger, we just 
            // replace a value, e.g.,  in a map from control (id?) to tagger.   Hmmm, is this 
            // significantly different than the above?  Maybe some additional locking with the 
            // event registration that we could ignore, since we assume single-threaded access to
            // the context object.
            view.onclick |> Option.iter (fun tagger -> 
                control.Click.Add (fun e -> 
                    processEvent context (tagger e) ))

            control
        
        interface Control<'Msg> with
            member view.create context = view.set (Button()) context :> obj

            member prev.update curr control context =
                let update prev (curr: Button<'Msg>) (control: Button) context = 
                    //control.Click.Subscribe
                    curr.set control context
                genericUpdate update prev curr control context

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

    let run (init: 's) (update: 'e -> 's -> ('s * Cmd<'e>)) (view: 's -> Window<'e>) =
        let context = Context()
        let app = new Application()
        let v = view init
        let w = (v :> Control<_>).create context
        app.Run((w :?> Window))

open View

[<EntryPoint>]
[<STAThread>]
let main argv = 

    let init = 1

    let update e s = s + e, Cmd.none

    let view s = 
        ui.window (
            width = 200.0, height = 200.0,
            content = ui.button (
                onclick = (fun _ -> 2),
                content = ui.text (string s) 
                )
            )

    run init update view