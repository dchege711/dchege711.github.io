namespace AoC2024;

public static partial class RedNosedReports
{
    public static int PartOne(string filePath)
    {
        var reports = GetReports(filePath);
        var numValidLevels = 0;
        foreach (var levels in reports)
        {
            var isValidLevel = true;
            Monotonicity? levelMonotonicity = null;
            var prevLevel = levels.First();
            foreach (var level in levels.Skip(1))
            {
                var monotonicity = GetMonotonicityIfValid(prevLevel, level, levelMonotonicity);
                levelMonotonicity ??= monotonicity;
                if (monotonicity is null)
                {
                    isValidLevel = false;
                    break;
                }
                
                prevLevel = level;
            }

            numValidLevels += isValidLevel ? 1 : 0;
        }
        return numValidLevels;
    }

    private static Monotonicity? GetMonotonicityIfValid(
        int a,
        int b,
        Monotonicity? targetMonotonicity)
    {
        var monotonicity = GetMonotonicityIfValid(a, b);
        if (targetMonotonicity is null)
            return monotonicity;
        else if (monotonicity == targetMonotonicity)
            return monotonicity;
        else
            return null;
    }

    private static Monotonicity? GetMonotonicityIfValid(int a, int b) =>
        (b - a) switch
        {
            (>= 1) and (<= 3) => Monotonicity.Increasing,
            (>= -3) and (<= -1) => Monotonicity.Decreasing,
            _ => null,
        };

    private enum Monotonicity { Increasing, Decreasing };
}
