using System.Reflection;

namespace AoC2024.Traces;

class Program
{
    static void Main(string[] args)
    {
        for (var i = 0; i < 10; i++)
        {
            var minCost = ClawContraption.PartOne(GetResourcePath("day-13-test.in.txt"));
            Console.WriteLine($"Min cost: {minCost}");
        }
    }
    
    private static string GetResourcePath(string fileName) =>
        Path.Combine(
            Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
                ?? throw new InvalidOperationException("Assembly path must not be null"),
            fileName);
}