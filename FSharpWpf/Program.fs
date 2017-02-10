
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
    type Brush =
        | Solid of Color

    type Font =
      { family: FontFamily option
        size: float
        stretch: FontStretch option
        style: FontStyle option
        weight: FontWeight option }

    type Control<'Msg> = interface end

    type Empty<'Msg> = | Empty
        with interface Control<'Msg>

    type Rectangle<'Msg> =
      { width: float
        height: float
        stroke: Brush option
        fill: Brush option
      }
      interface Control<'Msg>

    type Button<'Msg> =
      { width: float
        height: float
        content: Control<'Msg>
        onclick: (RoutedEvent -> 'Msg) option
      }
      interface Control<'Msg>

    type TextBlock<'Msg> =
      { text: string
        foreground: Brush option
        background: Brush option
        font: Font option }

    type ui =
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

[<EntryPoint>]
[<STAThread>]
let main argv = 
    
    let app = new Application()

    let button = new Button();
    button.VerticalAlignment <- VerticalAlignment.Top
    button.HorizontalAlignment <- HorizontalAlignment.Left
    button.Content <- "button"
    button.Click.Add <| fun e -> printf "%O\n" button.Template

    let window = new Window()
    window.Width <- 200.0
    window.Height <- 200.0
    window.Content <- button
    app.Run(window) |> ignore
    0
