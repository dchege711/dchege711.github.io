namespace AoC2024;

public static partial class HistorianHysteria
{
    public static int PartOne(LocationIds locationIds)
    {
        var (left, right) = locationIds;
        left.Sort();
        right.Sort();
        return left.Zip(right, (x1, x2) => int.Abs(x1 - x2)).Sum();
    }
}
