namespace AoC2024;

public partial class ClawContraption
{
    public static long PartTwo(string filePath) =>
        Parse(filePath)
            .AsParallel()
            .Select(config => new MachineConfig(config.A, config.B, AddOffset(config.Prize)))
            .Select(GetMinimumCost)
            .Where(cost => cost != long.MaxValue)
            .Sum();
    
    private static readonly long offset = 10_000_000_000_000;

    private static Vector AddOffset(Vector v) => new(v.X + offset, v.Y + offset);
}