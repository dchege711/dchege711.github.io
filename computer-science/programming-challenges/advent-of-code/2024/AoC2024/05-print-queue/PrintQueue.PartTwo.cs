using System.Collections.Immutable;

namespace AoC2024;

public partial class PrintQueue
{
    public int PartTwo() =>
        printJobs
            .Where(job => !IsValidJob(job))
            .Select(job => job.Sort(PageComparer))
            .Select(job => job[job.Count / 2])
            .Sum();

    private int PageComparer(int p1, int p2)
    {
        if (p1 == p2)
            return 0;
    
        orderingRules.TryGetValue(p1, out var p1Rules);
        if (p1Rules is not null && p1Rules.Contains(p2))
            return -1;

        orderingRules.TryGetValue(p2, out var p2Rules);
        if (p2Rules is not null && p2Rules.Contains(p1))
            return 1;
        
        throw new ArgumentException($"No valid ordering between {p1} and {p2}");
    }
}
