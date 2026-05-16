using System.Collections.Immutable;
using System.Text.RegularExpressions;

namespace AoC2024;

public partial class PrintQueue
{
    public readonly Dictionary<int, HashSet<int>> orderingRules = [];
    public readonly ImmutableList<ImmutableList<int>> printJobs = [];

    public PrintQueue(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;

        while ((line = inputReader.ReadLine()) != null)
        {
            var match = OrderingRuleRegex().Match(line);
            if (match is null || !match.Success)
                break;
            
            var p1 = int.Parse(match.Groups["p1"].Value);
            var p2 = int.Parse(match.Groups["p2"].Value);

            var existed = orderingRules.TryGetValue(p1, out var p1Rules);
            p1Rules ??= [];
            p1Rules.Add(p2);

            if (!existed)
                orderingRules.Add(p1, p1Rules);
        }

        var printJobsBuilder = printJobs.ToBuilder();
        while ((line = inputReader.ReadLine()) != null)
        {
            printJobsBuilder.Add(
                line.Split(",").Select(int.Parse).ToImmutableList());
        }
        printJobs = printJobsBuilder.ToImmutable();
    }

    [GeneratedRegex(@"(?<p1>\d+)\|(?<p2>\d+)")]
    private static partial Regex OrderingRuleRegex();
}
