using System.Text.RegularExpressions;

namespace AoC2024;

public static partial class HistorianHysteria
{
    public static LocationIds ParseLocationIds(string filePath)
    {
        List<int> left = [];
        List<int> right = [];

        using StreamReader inputReader = new(filePath);
        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            Match match = InputLineRegex().Match(line);
            if (!match.Success)
                throw new ArgumentException($"{line} is not well formatted");

            left.Add(int.Parse(match.Groups["left"].Value));
            right.Add(int.Parse(match.Groups["right"].Value));
        }

        return new(left, right);
    }

    public readonly record struct LocationIds(List<int> Left, List<int> Right);

    [GeneratedRegex(@"(?<left>\d+)\s+(?<right>\d+)")]
    private static partial Regex InputLineRegex();
}
