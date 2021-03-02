open System.Threading

open Avalonia
open Avalonia.Controls
open Avalonia.Logging

open Drophash.Avalonia

let BuildAvaloniaApp() =
  AppBuilderBase<AppBuilder>
    .Configure<App>()
    .UsePlatformDetect()
    .LogToTrace(LogEventLevel.Warning)

[<EntryPoint>]
let main arguments =
    Thread.CurrentThread.TrySetApartmentState(ApartmentState.STA)
    |> ignore

    BuildAvaloniaApp()
      .StartWithClassicDesktopLifetime(arguments)