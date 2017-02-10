
#I __SOURCE_DIRECTORY__
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "WindowsBase.dll"

open System
open System.Windows
open System.Windows.Input
open System.Windows.Controls
open System.Windows.Controls.Primitives

let style = new Style();
// Triggers, Resources, TargetType, Setters maybe aren't necessary?

// This seems to be necessary to at least have "content" and "children" properties, but what 
// methods must this have?  Another possibility is to encode all controls as union cases, but then 
// it's not "externally" extensible.  Perhaps a method that performs update given a previous
// ControlD value and a "live" Control?
type ControlD = interface end

type ButtonD<'Msg> =
  { width: float
    height: float
    content: ControlD

    click: RoutedEvent -> 'Msg
  }

(new Button()).Width