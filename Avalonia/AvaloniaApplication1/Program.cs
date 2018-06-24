using Avalonia;
using Avalonia.Logging.Serilog;
using System.Threading;

namespace AvaloniaApplication1
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            Thread.CurrentThread.TrySetApartmentState(ApartmentState.STA);
            BuildAvaloniaApp().Start<MainWindow>();
        }

        public static AppBuilder BuildAvaloniaApp()
            => AppBuilder.Configure<App>()
                .UsePlatformDetect()
                .LogToDebug();
    }
}