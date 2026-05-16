namespace AoC2024;

public static partial class HistorianHysteria
{
    public static int PartTwo(LocationIds locationIds)
    {
        var (left, right) = locationIds;

        // Objective: Use a functional approach. Avoid mutating values.
        var rightLookupTable = right
            .GroupBy(id => id)
            .ToDictionary(group => group.Key, group => group.Count());
        return (
            from id in left
            select id * rightLookupTable.GetValueOrDefault(id, 0)
        ).Sum();
    }
}
