using AoC2024.RestroomRedoubtDataTypes;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-14-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-14-test.in.txt")]
public sealed class Day14RestroomRedoubtTests
{
    [TestMethod]
    public void Parse()
    {
        var robots = RestroomRedoubt.Parse("day-14-sample.in.txt").ToList();
        robots.Count.Should().Be(12);
        robots[0].Should().Be(new Robot(new(0, 4), new(3, -3)));
        robots[4].Should().Be(new Robot(new(0, 0), new(1, 3)));
    }

    [TestMethod]
    public void RobotMove()
    {
        var (W, H) = (11, 7);

        var robot = new Robot(new(0, 0), new(-2, 4));
        robot.Move(1, W, H).Position.Should().Be(new Vector(9, 4));
        robot.Move(2, W, H).Position.Should().Be(new Vector(7, 1));
        robot.Move(3, W, H).Position.Should().Be(new Vector(5, 5));

        robot = new Robot(new(0, 0), new(11, 7));
        robot.Move(1, W, H).Position.Should().Be(new Vector(0, 0));
    }

    [TestMethod]
    [DataRow("day-14-sample.in.txt", 11, 7, 12)]
    [DataRow("day-14-test.in.txt", 101, 103, 232589280)]
    public void PartOne(
        string filePath, int areaWidth, int areaHeight, int expectedSafetyFactor)
    {
        var safetyFactor = RestroomRedoubt.PartOne(filePath, areaWidth, areaHeight);
        safetyFactor.Should().Be(expectedSafetyFactor);
    }
}
