namespace Drophash.Avalonia

open Avalonia.Controls
open Avalonia.Controls.Html
open Avalonia.Markup.Xaml

open System
open System.Globalization
open System.Resources

module UICommon =

    let GetResourceString (key:string) =
        let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resources = new ResourceManager("Drophash.Avalonia.Strings", executingAssembly)
        resources.GetString(key)


type MainWindow () as this =
    inherit Window()

    let mutable armed = false

    do this.InitializeComponent()
    member this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)
        //this.Title <- UICommon.GetResourceString "About"
        this.FindControl<TextBlock>("Program").Text <- "Drophash " + "version todo" //AssemblyVersionInformation.AssemblyFileVersion
        this.FindControl<TextBlock>("Description").Text <- UICommon.GetResourceString "ProgramDescription"

        let copyright = "copyright todo" //AssemblyVersionInformation.AssemblyCopyright
        this.FindControl<TextBlock>("Copyright").Text <- copyright

        let link = this.FindControl<HtmlLabel>("Link")
        link.Text <- """<center><a href="http://www.github.com/SteveGilham">""" +
                      UICommon.GetResourceString "WebsiteLabel" +
                      "</a></center>"

        link.PointerPressed |> Event.add (fun _ -> armed <- true)
        link.PointerLeave |> Event.add (fun _ -> armed <- false)
        link.PointerReleased
        |> Event.add (fun _ -> ())

        // Windows -- Process Start (url)
        // Mac -- ("open", url)
        // *nix -- ("xdg-open", url)
        //Application.Instance.Open("http://www.github.com/SteveGilham"))

        this.FindControl<TextBlock>("License").Text <- UICommon.GetResourceString "AboutDialog.License"
        this.FindControl<TextBlock>("MIT").Text <- String.Format(CultureInfo.InvariantCulture,
                                                                 UICommon.GetResourceString "License",
                                                                 copyright)