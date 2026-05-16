using AoC2024;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-03-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-03-sample.2.in.txt")]
[DeploymentItem("data/scratchpad/day-03-test.in.txt")]
public sealed class Day03MullItOverTests
{
    [TestMethod]
    [DataRow("day-03-sample.in.txt", 161)]
    [DataRow("day-03-test.in.txt", 183788984)]
    public void PartOne(string filePath, int expectedProduct)
    {
        MullItOver.PartOne(filePath).Should().Be(expectedProduct);
    }

    [TestMethod]
    [DataRow("day-03-sample.2.in.txt", 48)]
    [DataRow("day-03-test.in.txt", 62098619)]
    public void PartTwo(string filePath, int expectedProduct)
    {
        MullItOver.PartTwo(filePath).Should().Be(expectedProduct);
    }
}
