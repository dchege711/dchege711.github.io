namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-05-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-05-test.in.txt")]
public sealed class Day05PrintQueueTests
{
    [TestMethod]
    public void Parsing()
    {
        var printQueue = new PrintQueue("day-05-sample.in.txt");

        printQueue.orderingRules.TryGetValue(47, out var p47Rules);
        p47Rules.Should().NotBeNull();
        p47Rules.Should().BeEquivalentTo(new HashSet<int>([53, 13, 61, 29]));

        printQueue.printJobs.Count.Should().Be(6);
        printQueue.printJobs[0].Should().BeEquivalentTo([75,47,61,53,29]);
    }

    [TestMethod]
    [DataRow("day-05-sample.in.txt", 143)]
    [DataRow("day-05-test.in.txt", 5129)]
    public void PartOne(string filePath, int expectedSum)
    {
        var printQueue = new PrintQueue(filePath);
        printQueue.PartOne().Should().Be(expectedSum);
    }

    [TestMethod]
    [DataRow("day-05-sample.in.txt", 123)]
    [DataRow("day-05-test.in.txt", 4077)]
    public void PartTwo(string filePath, int expectedSum)
    {
        var printQueue = new PrintQueue(filePath);
        printQueue.PartTwo().Should().Be(expectedSum);
    }
}
