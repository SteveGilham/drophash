using System;
using System.Globalization;
using System.Resources;

using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Html;
using Avalonia.Input;
using Avalonia.Markup.Xaml;

namespace AvaloniaApplication1
{
    public class MainWindow : Window
    {
        private string GetResourceString(string key)
        {
            var executingAssembly = System.Reflection.Assembly.GetExecutingAssembly();
            var resources = new ResourceManager("AvaloniaApplication1.Strings", executingAssembly);
            return resources.GetString(key);
        }

        private bool armed;

        private System.Collections.Generic.List<IDisposable> sink = new System.Collections.Generic.List<IDisposable>();

        public MainWindow()
        {
            InitializeComponent();
#if DEBUG
            this.AttachDevTools();
#endif
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);

            // The content

            var tab = this.FindControl<TabItem>("Drop");
            tab.Header = GetResourceString("Drop");
            var zone = this.FindControl<TextBlock>("Zone");
            var scroll = this.FindControl<ScrollViewer>("DZ");
            var target = this.FindControl<Border>("Target");

            zone.MinWidth = scroll.Viewport.Width;
            zone.MinHeight = scroll.Viewport.Height;
            scroll.LayoutUpdated += (x, y) => { zone.MinWidth = scroll.Viewport.Width;
                zone.MinHeight = scroll.Viewport.Height; };

            //DragDrop.SetAllowDrop(target, true)
            sink.Add(
            target.AddHandler(DragDrop.DropEvent,
                              (x, e) =>
                                { if (e.Data.Contains(DataFormats.FileNames))
                                        zone.Text = String.Join(Environment.NewLine, e.Data.GetFileNames());
                                    else if (e.Data.Contains(DataFormats.Text))
                                        zone.Text = e.Data.GetText();
                                }));

        sink.Add(
        target.AddHandler(DragDrop.DragOverEvent, (x, e) =>
        {
            // Only allow Copy as Drop Operation.
            // Only allow if the dragged data contains text or filenames.
            //zone.Text < -zone.Text + (sprintf ".%A.\n" e.DragEffects)
            e.DragEffects = e.DragEffects &
                             ((e.Data.Contains(DataFormats.Text) ||
                                e.Data.Contains(DataFormats.FileNames)) ?
                                DragDropEffects.Copy : DragDropEffects.None);
        }));

            // "About"

            this.FindControl<TabItem>("About").Header = GetResourceString("About");
            this.FindControl<TextBlock>("Program").Text = "Drophash C# " + "version todo"; //AssemblyVersionInformation.AssemblyFileVersion
            this.FindControl<TextBlock>("Description").Text = GetResourceString("ProgramDescription");

            var copyright = "copyright todo"; //AssemblyVersionInformation.AssemblyCopyright
            this.FindControl<TextBlock>("Copyright").Text = copyright;

            var link = this.FindControl<HtmlLabel>("Link");
            link.Text = "<center><a href=\"http://www.github.com/SteveGilham\">" +
                GetResourceString("WebsiteLabel") +
                "</a></center>";

            link.PointerPressed += (x, e) => { armed = true; };
            link.PointerLeave += (x, e) => { armed = false; };
            //link.PointerReleased
            //|> Event.add(fun _-> ())

            // Windows -- Process Start (url)
            // Mac -- ("open", url)
            // *nix -- ("xdg-open", url)
            //Application.Instance.Open("http://www.github.com/SteveGilham"))

            this.FindControl<TextBlock>("License").Text = GetResourceString("AboutDialog.License");
            this.FindControl<TextBlock>("MIT").Text = String.Format(CultureInfo.InvariantCulture,
                                                                     GetResourceString("License"),
                                                                     copyright);
        }
    }
}
