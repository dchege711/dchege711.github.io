using AoC2024;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-01-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-01-test.in.txt")]
public sealed class Day01HistorianHysteriaTests
{
    [TestMethod]
    [DataRow("day-01-sample.in.txt", 11)]
    [DataRow("day-01-test.in.txt", 1941353)]
    public void HistorianHysteria01(string filePath, int expected)
    {
        var locationIds = HistorianHysteria.ParseLocationIds(filePath);
        var actual = HistorianHysteria.PartOne(locationIds);
        actual.Should().Be(expected);
    }

    [TestMethod]
    [DataRow("day-01-sample.in.txt", 31)]
    [DataRow("day-01-test.in.txt", 22539317)]
    public void HistorianHysteria02(string filePath, int expected)
    {
        var locationIds = HistorianHysteria.ParseLocationIds(filePath);
        var actual = HistorianHysteria.PartTwo(locationIds);
        actual.Should().Be(expected);
    }
}
