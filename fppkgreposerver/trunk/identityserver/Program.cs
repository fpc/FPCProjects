using System;
using System.IO;
using Microsoft.AspNetCore.Hosting;

namespace IdentityServerWithAspNetIdentity
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.Title = "IdentityServer";

            var host = new WebHostBuilder()
                .UseKestrel()
                .UseUrls("http://0.0.0.0:5000")
                .UseContentRoot(Directory.GetCurrentDirectory())
                .UseStartup<Startup>()
                .Build();

            host.Run();
        }
    }
}
