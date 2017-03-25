

let run ui events =
    ui.view.Subscribe (fun view -> printf "UI event: %A\n" view) |> ignore
    events |> List.iter (fun e ->
        ui.input.OnNext (e))
