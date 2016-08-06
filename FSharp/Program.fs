module TodoElmStyle

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Shapes
open System.Windows.Media
open XamlBuilders

type Task = {
    Text: String;
    Completed: Boolean
}

type Todos = {
    Tasks: List<Task>;
    Filter: Boolean
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

let headerView (model: Todos) dispatch =
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

let taskView (model: Task) dispatch =
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

let bodyView (model: Todos) dispatch =
    StackPanel(Orientation = Orientation.Vertical) {
        children (Seq.map (fun t -> taskView t dispatch) model.Tasks)
    }

let footerView (model: Todos) dispatch =
    StackPanel(Orientation = Orientation.Horizontal) {
        children [
            TextBlock(Text = "Show Completed")
            CheckBox()
        ]
    }
    
let view (model: Todos) dispatch =
    Grid(Margin = Thickness(5.0)) {
        rows [
            RowDefinition(Height = GridLength.Auto)
            RowDefinition()
            RowDefinition(Height = GridLength.Auto)
        ]

        children [
            headerView model dispatch { set [Grid.RowProperty, 0] }
            bodyView model dispatch { set [Grid.RowProperty, 1] }
            footerView model dispatch { set [Grid.RowProperty, 2] }
        ]
    }

let initialState: Todos = { Tasks = []; Filter = true }

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let mainWindow = Window()
    let mutable state = initialState
    
    let rec dispatch msg =
        state <- update msg state
        mainWindow.Content <- view state dispatch

    mainWindow.Content <- view state dispatch
    
    let application = Application()
    application.Run(mainWindow)
