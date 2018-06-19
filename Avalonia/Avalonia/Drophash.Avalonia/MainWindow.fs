namespace Drophash.Avalonia

open Avalonia.Controls
open Avalonia.Controls.Html
open Avalonia.Input
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

        // The content

        this.FindControl<TabItem>("Drop").Header <- UICommon.GetResourceString "Drop"
        let zone = this.FindControl<TextBlock>("Zone")
        let scroll = this.FindControl<ScrollViewer>("DZ")
        zone.MinWidth <- scroll.Viewport.Width
        zone.MinHeight <- scroll.Viewport.Height
        scroll.LayoutUpdated |> Event.add ( fun _ -> zone.MinWidth <- scroll.Viewport.Width
                                                     zone.MinHeight <- scroll.Viewport.Height)

        this.AddHandler(DragDrop.DropEvent,
                        new EventHandler<DragEventArgs>(fun _ e -> 
                            if e.Data.Contains(DataFormats.Text) then
                                zone.Text <- e.Data.GetText();
                            if e.Data.Contains(DataFormats.FileNames) then
                                zone.Text <- String.Join(Environment.NewLine, e.Data.GetFileNames())   
                        )) |> ignore
        this.AddHandler(DragDrop.DragOverEvent, 
                        new EventHandler<DragEventArgs>(fun _ e -> // Only allow Copy as Drop Operation.
                            zone.Text <- zone.Text + (sprintf ".%A.\n" e.DragEffects)
                            e.DragEffects <- e.DragEffects &&& DragDropEffects.Copy

                            // Only allow if the dragged data contains text or filenames.
                            if (e.Data.Contains(DataFormats.Text) || e.Data.Contains(DataFormats.FileNames)) |> not then
                                e.DragEffects <- DragDropEffects.None)) |> ignore

        // "About"

        this.FindControl<TabItem>("About").Header <- UICommon.GetResourceString "About"
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