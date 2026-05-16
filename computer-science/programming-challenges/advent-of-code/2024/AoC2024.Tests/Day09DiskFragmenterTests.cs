namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-09-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-09-sample-2.in.txt")]
[DeploymentItem("data/scratchpad/day-09-sample-3.in.txt")]
[DeploymentItem("data/scratchpad/day-09-sample-4.in.txt")]
[DeploymentItem("data/scratchpad/day-09-test.in.txt")]
public sealed class Day09DiskFragmenterTests
{
    [TestMethod]
    public void Parse()
    {
        var diskMap = DiskFragmenter.Parse("day-09-sample.in.txt");
        diskMap.ToArray().Should().BeEquivalentTo([
            2, 3, 3, 3, 1, 3, 3, 1, 2, 1, 4, 1, 4, 1, 3, 1, 4, 0, 2]);
    }

    [TestMethod]
    [DataRow("day-09-sample.in.txt", 1928L)]
    [DataRow("day-09-sample-2.in.txt", 60L)]
    [DataRow("day-09-sample-3.in.txt", 12L)]
    [DataRow("day-09-test.in.txt", 6421128769094L)]
    public void PartOne(string filePath, long expectedChecksum)
    {
        var diskMap = DiskFragmenter.Parse(filePath);
        var checksum = DiskFragmenter.PartOne(diskMap);
        checksum.Should().Be(expectedChecksum);
    }

    [TestMethod]
    [DataRow("day-09-sample.in.txt", 2858L)]
    [DataRow("day-09-sample-2.in.txt", 132L)]
    [DataRow("day-09-sample-3.in.txt", 12L)]
    [DataRow("day-09-sample-4.in.txt", 31L)]
    [DataRow("day-09-test.in.txt", 6448168620520L)]
    public void PartTwo(string filePath, long expectedChecksum)
    {
        var diskMap = DiskFragmenter.Parse(filePath);
        var checksum = DiskFragmenter.PartTwo(diskMap);
        checksum.Should().Be(expectedChecksum);
    }
}
