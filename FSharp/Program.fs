open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives

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

type PlusExtension = PlusExtension with
    static member inline (=>) (x, PlusExtension) = 
        fun y -> x + y

    static member (=>) (x: Grid, PlusExtension) = 
        fun y -> x.RowDefinitions.Add(y)
                 x

    //static member (=>) (x: Grid, PlusExtension) = 
      //  fun y -> x.ColumnDefinitions.Add(y)
                 x

    static member (=>) (x:PlusExtension, PlusExtension) = 
        fun PlusExtension -> x

let inline (+) x y = (x => PlusExtension) y

type Panel with
    member x.Containing(children: UIElement[]) =
        for child in children do
            x.Children.Add(child) |> ignore
        x

type Grid with
    member x.Row(?Height: GridLength, ?MinHeight: double, ?MaxHeight: double, ?SharedSizeGroup: String) =
        let row = RowDefinition()
        if (Height.IsSome) then row.Height <- Height.Value
        if (MinHeight.IsSome) then row.MinHeight <- MinHeight.Value
        if (MaxHeight.IsSome) then row.MaxHeight <- MaxHeight.Value
        if (SharedSizeGroup.IsSome) then row.SharedSizeGroup <- SharedSizeGroup.Value
        x.RowDefinitions.Add(row)
        x

    member x.Column(?Width: GridLength, ?MinWidth: double, ?MaxWidth: double, ?SharedSizeGroup: String) =
        let col = ColumnDefinition()
        if (Width.IsSome) then col.Width <- Width.Value
        if (MinWidth.IsSome) then col.MinWidth <- MinWidth.Value
        if (MaxWidth.IsSome) then col.MaxWidth <- MaxWidth.Value
        if (SharedSizeGroup.IsSome) then col.SharedSizeGroup <- SharedSizeGroup.Value
        x.ColumnDefinitions.Add(col)
        x
    
let headerView (model: Todos) =
    let textBox = TextBox()
    textBox.KeyDown.Add(fun e -> 
        if e.Key = Input.Key.Enter then
            e.Handled <- true
        )
    Grid()
        .Column(Width = GridLength.Auto)
        .Column()
        .Containing(
            [| 
                TextBlock(Text = "Add a todo")
                textBox
            |])

let view (model: Todos) =
    let grid = Grid(Margin = Thickness(5.0)) 
             + RowDefinition(Height = GridLength.Auto)
             + RowDefinition()
             + RowDefinition(Height = GridLength.Auto)
    let header = headerView(model)
    header.SetValue(Grid.RowProperty, 0 + 0)
    let body = bodyView(model)
    body.SetValue(Grid.RowProperty, 1)
    let footer = footerView(model)
    footer.SetValue(Grid.RowProperty, 2)



[<STAThread>]
[<EntryPoint>]
let main argv = 
    let mainWindow = Window()
    let application = Application()
    application.Run(mainWindow)
