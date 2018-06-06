namespace Drophash.Avalonia

open Avalonia
open Avalonia.Markup.Xaml

type App() =
    inherit Application()
    override this.Initialize() =
        AvaloniaXamlLoader.Load(this)
