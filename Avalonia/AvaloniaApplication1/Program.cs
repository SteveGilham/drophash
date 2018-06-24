using Avalonia;
using Avalonia.Logging.Serilog;

namespace AvaloniaApplication1
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            BuildAvaloniaApp().Start<MainWindow>();
        }

        public static AppBuilder BuildAvaloniaApp()
            => AppBuilder.Configure<App>()
                .UsePlatformDetect()
                .LogToDebug();
    }
}