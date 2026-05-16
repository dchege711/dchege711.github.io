namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-12-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-12-2-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-12-3-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-12-4-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-12-5-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-12-test.in.txt")]
public sealed class Day12GardenGroupsTests
{
    [TestMethod]
    [DataRow("day-12-sample.in.txt", 1930)]
    [DataRow("day-12-2-sample.in.txt", 140)]
    [DataRow("day-12-3-sample.in.txt", 772)]
    [DataRow("day-12-4-sample.in.txt", 32)]
    [DataRow("day-12-test.in.txt", 1518548)]
    public void TotalPrice(string filePath, int expectedPrice)
    {
        GardenGroups gardenGroups = new(filePath);
        gardenGroups.ComputeTotalFencingPrice().Should().Be(expectedPrice);
    }

    [TestMethod]
    [DataRow("day-12-sample.in.txt", 1206)]
    [DataRow("day-12-2-sample.in.txt", 80)]
    [DataRow("day-12-3-sample.in.txt", 436)]
    [DataRow("day-12-4-sample.in.txt", 16)]
    [DataRow("day-12-5-sample.in.txt", 368)]
    [DataRow("day-12-test.in.txt", 909564)]
    public void TotalDiscountedPrice(string filePath, int expectedPrice)
    {
        GardenGroups gardenGroups = new(filePath);
        gardenGroups.ComputeTotalDiscountedFencingPrice().Should().Be(expectedPrice);
    }
}
