using Coordinate = AoC2024.ResonantCollinearity.Coordinate;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-08-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-08-test.in.txt")]
public sealed class Day08ResonantCollinearityTests
{
    [TestMethod]
    public void Parse()
    {
        var resonantCollinearity = new ResonantCollinearity("day-08-sample.in.txt");

        resonantCollinearity.antennasMap.RowCount.Should().Be(12);
        resonantCollinearity.antennasMap.ColCount.Should().Be(12);
        resonantCollinearity.antennasMap.AntennasByFrequency.Count.Should().Be(2);

        resonantCollinearity.antennasMap.AntennasByFrequency['0'].Should().BeEquivalentTo(
            new List<Coordinate>([new(1, 8), new(2, 5), new(3, 7), new(4, 4)]));

        resonantCollinearity.antennasMap.AntennasByFrequency['A'].Should().BeEquivalentTo(
            new List<Coordinate>([new(5, 6), new(8, 8), new(9, 9)]));
    }

    [TestMethod]
    [DataRow("day-08-sample.in.txt", 14)]
    [DataRow("day-08-test.in.txt", 273)]
    public void PartOne(string filePath, int expectedNumDistinctPositions)
    {
        var resonantCollinearity = new ResonantCollinearity(filePath);
        var numDistinctPositions = resonantCollinearity.PartOne();
        numDistinctPositions.Should().Be(expectedNumDistinctPositions);
    }

    [TestMethod]
    [DataRow("day-08-sample.in.txt", 34)]
    [DataRow("day-08-test.in.txt", 1017)]
    public void PartTwo(string filePath, int expectedNumDistinctPositions)
    {
        var resonantCollinearity = new ResonantCollinearity(filePath);
        var numDistinctPositions = resonantCollinearity.PartTwo();
        numDistinctPositions.Should().Be(expectedNumDistinctPositions);
    }
}
