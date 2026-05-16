using System.Text.RegularExpressions;
using AoC2024.RestroomRedoubtDataTypes;

namespace AoC2024;

public partial class RestroomRedoubt
{
    public static IEnumerable<Robot> Parse(string filePath)
    {
        using var inputReader = new StreamReader(filePath);
        while (inputReader.Peek() != -1)
        {
            Match match = RobotLineRegex().Match(
                inputReader.ReadLine()
                     ?? throw new ArgumentException("line must not be null"));
            yield return new(
                new(int.Parse(match.Groups["X"].Value), int.Parse(match.Groups["Y"].Value)),
                new(int.Parse(match.Groups["dX"].Value), int.Parse(match.Groups["dY"].Value)));
        }
    }

    [GeneratedRegex(@"p=(?<X>[\d-]+),(?<Y>[\d-]+) v=(?<dX>[\d-]+),(?<dY>[\d-]+)", default, 50)]
    private static partial Regex RobotLineRegex();
}