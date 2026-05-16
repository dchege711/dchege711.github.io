using AoC2024.RestroomRedoubtDataTypes;

namespace AoC2024;

public partial class RestroomRedoubt
{
    public static int PartOne(string filePath, int areaWidth, int areaHeight) =>
        Parse(filePath)
            .Select(robot => robot.Move(100, areaWidth, areaHeight))
            .Select(robot => robot.Position)
            .GroupBy(v => v.GetQuadrant(areaWidth, areaHeight))
            .Where(g => g.Key != Quadrant.kBoundary)
            .Select(g => g.Count())
            .Aggregate(1, (acc, count) => acc * count);
}