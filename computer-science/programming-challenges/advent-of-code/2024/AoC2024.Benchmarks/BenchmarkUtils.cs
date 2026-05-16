using System.Reflection;

namespace AoC2024.Benchmarks;

internal static class BenchmarkUtils
{
    public static string GetResourcePath(string fileName) =>
        Path.Combine(
            Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
                ?? throw new InvalidOperationException("Assembly path must not be null"),
            fileName);
}