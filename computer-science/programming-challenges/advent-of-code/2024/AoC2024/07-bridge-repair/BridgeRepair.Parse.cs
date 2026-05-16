using System.Collections.Immutable;
using System.Text.RegularExpressions;

namespace AoC2024;

public partial class BridgeRepair
{
    public static IEnumerable<CalibrationEquation> Parse(string filePath)
    {
        using var inputReader = new StreamReader(filePath);

        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            var numbers = InputLineRegex.Matches(line)
                .Select(match => long.Parse(match.Value));
            yield return new(numbers.First(), numbers.Skip(1).ToImmutableList());
        }
    }

    public readonly record struct CalibrationEquation(
        long Result, ImmutableList<long> Operands);

    private static Regex InputLineRegex = NumbersRegex();

    [GeneratedRegex(@"\d+")]
    static private partial Regex NumbersRegex();
}
