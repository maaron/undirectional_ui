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
    
let view (model: Todos): UIElement =
    Grid()

[<STAThread>]
[<EntryPoint>]
let main argv = 
    let mainWindow = Window()
    let application = Application()
    application.Run(mainWindow)
