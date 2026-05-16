namespace AoC2024;

public partial class ClawContraption
{
    public static long PartOne(string filePath) =>
        Parse(filePath)
            .AsParallel()
            .Select(GetMinimumCost)
            .Where(cost => cost != long.MaxValue)
            .Sum();
}