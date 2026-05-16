using AoC2024;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-04-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-04-test.in.txt")]
public sealed class Day04CeresSearchTests
{
    [TestMethod]
    [DataRow("day-04-sample.in.txt", 18)]
    [DataRow("day-04-test.in.txt", 2496)]
    public void PartOne(string filePath, int expectedNumOccurrences)
    {
        var ceresSearch = new CeresSearch(filePath);
        ceresSearch.PartOne().Should().Be(expectedNumOccurrences);
    }

    [TestMethod]
    [DataRow("day-04-sample.in.txt", 9)]
    [DataRow("day-04-test.in.txt", 1967)]
    public void PartTwo(string filePath, int expectedNumOccurrences)
    {
        var ceresSearch = new CeresSearch(filePath);
        ceresSearch.PartTwo().Should().Be(expectedNumOccurrences);
    }
}
