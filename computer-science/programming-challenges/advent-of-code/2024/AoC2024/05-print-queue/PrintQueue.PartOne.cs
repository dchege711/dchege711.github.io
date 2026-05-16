using System.Collections.Immutable;

namespace AoC2024;

public partial class PrintQueue
{
    public int PartOne() =>
        printJobs
            .Where(IsValidJob)
            .Select(job => job[job.Count / 2])
            .Sum();

    private bool IsValidJob(ImmutableList<int> job) =>
        job
            .Select(
                (p1, idx) => idx == job.Count - 1 || IsValidOrderedPair(p1, job[idx + 1]))
            .All(isValidPair => isValidPair);

    private bool IsValidOrderedPair(int p1, int p2) =>
        orderingRules.TryGetValue(p1, out var rules) && rules.Contains(p2);
}
