using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Markup.Xaml;
using System;

namespace AvaloniaApplication1
{
    public class MainWindow : Window
    {
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

            var zone = this.FindControl<TextBlock>("Zone");
            var target = this.FindControl<Border>("Target");

            sink.Add(
            target.AddHandler(DragDrop.DropEvent,
                              (x, e) =>
                                {
                                    if (e.Data.Contains(DataFormats.FileNames))
                                        zone.Text = String.Join(Environment.NewLine, 
                                                    e.Data.GetFileNames());
                                    else if (e.Data.Contains(DataFormats.Text))
                                        zone.Text = e.Data.GetText();
                                }));

            sink.Add(
            target.AddHandler(DragDrop.DragOverEvent, (x, e) =>
            {
                zone.Text = e.DragEffects.ToString();
                e.DragEffects = e.DragEffects &
                                 ((e.Data.Contains(DataFormats.Text) ||
                                    e.Data.Contains(DataFormats.FileNames)) ?
                                    (DragDropEffects.Copy | DragDropEffects.Link) : DragDropEffects.None);
            }));
        }
    }
}