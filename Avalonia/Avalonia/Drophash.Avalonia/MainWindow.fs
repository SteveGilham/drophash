namespace Drophash.Avalonia

open Avalonia.Controls
open Avalonia.Controls.Html
open Avalonia.Input
open Avalonia.Markup.Xaml

open System
open System.Globalization
open System.IO
open System.Resources
open System.Security.Cryptography

module UICommon =

    let GetResourceString (key:string) =
        let executingAssembly = System.Reflection.Assembly.GetExecutingAssembly()
        let resources = new ResourceManager("Drophash.Avalonia.Strings", executingAssembly)
        resources.GetString(key)

type MainWindow () as this =
    inherit Window()

    let mutable first = true

    do this.InitializeComponent()

    member this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)

        // The content

        let tab = this.FindControl<TabItem>("Drop")
        tab.Header <- UICommon.GetResourceString "Drop"
        let zone = this.FindControl<TextBlock>("Zone")
        let scroll = this.FindControl<ScrollViewer>("DZ")
        let target = this.FindControl<Border>("Target")

        target.MinWidth <- scroll.Viewport.Width
        target.MinHeight <- scroll.Viewport.Height
        scroll.LayoutUpdated |> Event.add ( fun _ -> target.MinWidth <- scroll.Viewport.Width
                                                     target.MinHeight <- scroll.Viewport.Height)
        zone.Text <- UICommon.GetResourceString "Zone"

        DragDrop.SetAllowDrop(target, true)
        let buffer = Array.init 4096 (fun _ -> 0uy)

        target.AddHandler(DragDrop.DropEvent,
                        new EventHandler<DragEventArgs>(fun _ e ->
                            if e.Data.Contains(DataFormats.FileNames) then
                                if first then
                                  zone.Text <- String.Empty
                                  first <- false
                                let files = e.Data.GetFileNames() |> Seq.toArray
                                zone.Text <- zone.Text + (if files.Length = 1 then
                                                           UICommon.GetResourceString "Singular"
                                                          else String.Format(UICommon.GetResourceString "Multiple", files.Length)) +
                                                         Environment.NewLine
                                files
                                |> Seq.iter (fun n ->
                                   use stream = File.OpenRead n
                                   use md5 = MD5.Create()
                                   use sha = SHA1.Create()
                                   use sha2 = SHA256.Create()
                                   let h = [md5 :> HashAlgorithm
                                            sha :> HashAlgorithm
                                            sha2 :> HashAlgorithm]
                                   h |> List.iter (fun x -> x.Initialize())
                                   let rec proc () =
                                     let r = stream.Read(buffer, 0, 4096)
                                     if r = 4096 then
                                        h |> List.iter (fun x -> x.TransformBlock(buffer, 0, r, buffer, 0) |> ignore)
                                        proc()
                                     else
                                        h |> List.iter (fun x -> x.TransformFinalBlock(buffer, 0, r) |> ignore)
                                   proc()
                                   zone.Text <- zone.Text + n + Environment.NewLine
                                   h
                                   |> List.zip ["MD5       "
                                                "SHA-1     "
                                                "SHA2-256  "]
                                   |> List.iter (fun (s,x) -> zone.Text <- zone.Text + s +
                                                                           String.Join (" ", x.Hash
                                                                              |> Seq.pairwise
                                                                              |> Seq.map (fun (x,y) -> x.ToString("x2") + y.ToString("x2")))
                                                                              + Environment.NewLine)
                                   zone.Text <- zone.Text + Environment.NewLine + Environment.NewLine
                                )
                        ))

        let inProcess = new EventHandler<DragEventArgs>(fun _ e ->
                            // Only allow Copy as Drop Operation.
                            // Only allow if the dragged data contains text or filenames.
                            e.DragEffects <- e.DragEffects &&&
                                             if e.Data.Contains(DataFormats.FileNames) then
                                                DragDropEffects.Copy
                                             else
                                                DragDropEffects.None)
        target.AddHandler(DragDrop.DragOverEvent, inProcess)

        // "About"

        this.FindControl<TabItem>("About").Header <- UICommon.GetResourceString "About"
        this.FindControl<TextBlock>("Program").Text <- "Drophash " + "version todo" //AssemblyVersionInformation.AssemblyFileVersion
        this.FindControl<TextBlock>("Description").Text <- UICommon.GetResourceString "ProgramDescription"

        let copyright = "copyright todo" //AssemblyVersionInformation.AssemblyCopyright
        this.FindControl<TextBlock>("Copyright").Text <- copyright

        let link = this.FindControl<TextBlock>("Link")
        link.Text <- UICommon.GetResourceString "WebsiteLabel"
        let linkButton = this.FindControl<Button>("LinkButton")

        linkButton.Click
        |> Event.add
             (fun _ ->
               Avalonia.Dialogs.AboutAvaloniaDialog.OpenBrowser
                 "http://www.github.com/SteveGilham/drophash")

        this.FindControl<TextBlock>("License").Text <- UICommon.GetResourceString "AboutDialog.License"
        this.FindControl<TextBlock>("MIT").Text <- String.Format(CultureInfo.InvariantCulture,
                                                                 UICommon.GetResourceString "License",
                                                                 copyright)