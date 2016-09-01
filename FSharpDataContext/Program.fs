module TodoElmStyle

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Shapes
open System.Windows.Media
open System.Windows.Threading
open System.Diagnostics
open XamlBuilders

type Task = {
    Text: String;
    Completed: Boolean
}

type Todos = {
    Tasks: List<Task>;
    Filter: Boolean;
}

type Msg = 
    | Add of Task
    | Remove of Task
    | Filter of Boolean
    | Complete of Task * Boolean

let update (msg: Msg) (model: Todos): Todos =
    match msg with
    | Add task -> { model with Tasks = model.Tasks @ [task]}
    | Remove task -> { model with Tasks = List.filter ((<>) task) model.Tasks}
    | Filter filter -> { model with Filter = filter }
    | Complete (task, complete) -> 
        let completeTask t = if t = task then { t with Completed = complete } else t
        { model with Tasks = List.map completeTask model.Tasks }

type TemplateBuilder(model) =
    member x.Return(element: FrameworkElement) = x

    member x.Bind(a: 'a) = a

type BoundControl = {
    element: UIElement
    update: Todos -> unit
}

type Template = Todos -> BoundControl

type Dispatch = Msg -> unit
type MakeTemplate = Dispatch -> Todos -> (Todos -> unit)
type MakeElement = Dispatch -> (Todos -> unit)

let bindModel element updateElement: BoundControl =
    { element = element; update = fun model -> updateElement element model }

let staticModel element: BoundControl =
    { element = element; update = fun _ -> () }

let headerTemplate (d: Dispatch): BoundControl =
    let textBox = TextBox()
    {
        element = textBox
        update = fun m -> textBox.Text <- m.ToString()
    }

let headerTemplate2 dispatch =
    bindModel (TextBox()) (fun tb todos -> tb.Text <- todos.ToString())

let headerView dispatch =
    staticModel (
        Grid() {
            columns [
                ColumnDefinition(Width = GridLength.Auto)
                ColumnDefinition()
            ]
            children [
                TextBlock(Text = "Add a todo") {
                    set [Grid.ColumnProperty, 0]
                }
                TextBox() {
                    set [Grid.ColumnProperty, 1]

                    onLoaded (fun t e -> t.Focus())

                    onKeyDown (fun t e -> 
                        if e.Key = Input.Key.Enter then
                            e.Handled <- true
                            Add { Text = t.Text; Completed = false } |> dispatch)
                }
            ]
        }
    )

let taskView dispatch =
    bindModel (
        ItemsControl(
            ItemsPanel = ItemsPanelTemplate(Template = StackPanel(Orientation = Orientation.Vertical)),
            ItemTemplate = DataTemplate()
        )
    ) (fun e model -> ())

    #if false
    StackPanel(Orientation = Orientation.Horizontal) {
        children [
            CheckBox(
                Margin = Thickness(5.0), 
                IsChecked = Nullable<bool>(model.Completed)
            ) {
                onClick (fun c e -> Complete (model, not model.Completed) |> dispatch)
            }
            TextBlock(
                Margin = Thickness(5.0),
                Text = model.Text,
                TextDecorations = if model.Completed 
                    then TextDecorations.Strikethrough
                    else null
            )
            Button(
                Margin = Thickness(5.0),
                HorizontalContentAlignment = HorizontalAlignment.Center,
                VerticalContentAlignment = VerticalAlignment.Center,
                Content = Grid() {
                    children [
                        Line(
                            Stroke = Brushes.Red,
                            StrokeThickness = 2.0,
                            X1 = 0.0, Y1 = 0.0,
                            X2 = 10.0, Y2 = 10.0
                        )
                        Line(
                            StrokeThickness = 2.0,
                            Stroke = Brushes.Red,
                            X1 = 0.0, Y1 = 10.0,
                            X2 = 10.0, Y2 = 0.0
                        )
                    ]
                }
            ) {
                onClick (fun c e -> Remove model |> dispatch)
            }
        ]
    } :> UIElement
    #endif

let profile x =
    let watch = Stopwatch()
    watch.Start()
    let r = x()
    watch.Stop()
    Trace.WriteLine(watch.ElapsedMilliseconds)
    r

let bodyView dispatch model =
    ScrollViewer(
        Content = 
            StackPanel(Orientation = Orientation.Vertical) {
                children (List.map (taskView dispatch) model.Tasks)
            })

let footerView dispatch model =
    StackPanel(Orientation = Orientation.Horizontal) {
        children [
            TextBlock(Text = "Show Completed")
            CheckBox()
        ]
    }
    
let view dispatch model =
    Grid(Margin = Thickness(5.0)) {
        rows [
            RowDefinition(Height = GridLength.Auto)
            RowDefinition()
            RowDefinition(Height = GridLength.Auto)
        ]

        children [
            headerView dispatch model { set [Grid.RowProperty, 0] }
            bodyView dispatch model { set [Grid.RowProperty, 1] }
            footerView dispatch model { set [Grid.RowProperty, 2] }
        ]
    }

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let mainWindow = Window()

    let mutable state: Todos = { 
        Tasks = [for i in 1 .. 10 -> { Text = i.ToString(); Completed = false }]; 
        Filter = true;
    }
    
    let rec dispatch msg =
        state <- profile (fun () -> update msg state)
        mainWindow.DataContext <- state

    mainWindow.Content <- view dispatch state
    mainWindow.DataContext <- state
    
    let application = Application()
    application.Run(mainWindow)
