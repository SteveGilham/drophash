open System.Threading

open Avalonia
open Avalonia.Controls
open Avalonia.Logging
open Avalonia.Logging.Serilog

open Drophash.Avalonia

let BuildAvaloniaApp() =
    AppBuilderBase<AppBuilder>.Configure<App>().UsePlatformDetect().LogToDebug(LogEventLevel.Warning)

[<EntryPoint>]
let main _ =
    Thread.CurrentThread.TrySetApartmentState(ApartmentState.STA)
    |> ignore

    AppBuilder.Configure<App>()
        .UsePlatformDetect()
        .Start<MainWindow>()
    0