namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-06-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-06-test.in.txt")]
public sealed class Day06GuardGallivantTests
{
    [TestMethod]
    public void Parsing()
    {
        var guardGallivant = new GuardGallivant("day-06-sample.in.txt");
        var (RowCount, ColCount, Obstacles) = guardGallivant.AreaMap;

        RowCount.Should().Be(10);
        ColCount.Should().Be(10);

        Obstacles.Contains(new GuardGallivant.Coordinate(3, 2)).Should().BeTrue();
        Obstacles.Contains(new GuardGallivant.Coordinate(9, 9)).Should().BeFalse();

        guardGallivant.StartingPosition.Should().BeEquivalentTo(
            new GuardGallivant.Visit(
                new GuardGallivant.Coordinate(6, 4),
                GuardGallivant.Orientation.Up));
    }

    [TestMethod]
    [DataRow("day-06-sample.in.txt", 41)]
    [DataRow("day-06-test.in.txt", 5101)]
    public void PartOne(string filePath, int expectedNumDistinctPositions)
    {
        var guardGallivant = new GuardGallivant(filePath);
        guardGallivant.PartOne().Should().Be(expectedNumDistinctPositions);
    }

    [TestMethod]
    [DataRow("day-06-sample.in.txt", 6)]
    [DataRow("day-06-test.in.txt", 1951)]
    public void PartTwo(string filePath, int expectedNumDistinctPositions)
    {
        var guardGallivant = new GuardGallivant(filePath);
        guardGallivant.PartTwo().Should().Be(expectedNumDistinctPositions);
    }
}
