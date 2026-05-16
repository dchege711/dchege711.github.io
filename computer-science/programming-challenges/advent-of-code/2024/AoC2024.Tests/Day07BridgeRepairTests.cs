using System.Collections.Immutable;

namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-07-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-07-test.in.txt")]
public sealed class Day07BridgeRepairTests
{
    [TestMethod]
    public void Parse()
    {
        var calibrations = BridgeRepair.Parse("day-07-sample.in.txt");

        calibrations.Count().Should().Be(9);

        calibrations.First().Should().BeEquivalentTo(
            new BridgeRepair.CalibrationEquation(
                190, ImmutableList.Create([10L, 19L])));

        calibrations.Last().Should().BeEquivalentTo(
            new BridgeRepair.CalibrationEquation(
                292, ImmutableList.Create([11L, 6L, 16L, 20L])));
    }

    [TestMethod]
    [Ignore]
    [DataRow("day-07-sample.in.txt", 3749L)]
    [DataRow("day-07-test.in.txt", 2941973819040L)]
    public void PartOneIterative(string filePath, long expectedCalibrationTotal)
    {
        var calibrations = BridgeRepair.Parse(filePath);
        var calibrationTotal = BridgeRepair.TotalCalibrationResult(calibrations);
        calibrationTotal.Should().Be(expectedCalibrationTotal);
    }

    [TestMethod]
    [Ignore]
    [DataRow("day-07-sample.in.txt", 11387L)]
    [DataRow("day-07-test.in.txt", 249943041417600L)]
    public void PartTwoIterative(string filePath, long expectedCalibrationTotal)
    {
        var calibrations = BridgeRepair.Parse(filePath);
        var calibrationTotal = BridgeRepair.TotalCalibrationResultWithConcat(calibrations);
        calibrationTotal.Should().Be(expectedCalibrationTotal);
    }

    [TestMethod]
    [DataRow("day-07-sample.in.txt", 3749L)]
    [DataRow("day-07-test.in.txt", 2941973819040L)]
    public void PartOne(string filePath, long expectedCalibrationTotal)
    {
        var calibrations = BridgeRepair.Parse(filePath);
        var calibrationTotal = BridgeRepair.TotalCalibrationResultRecursive(calibrations);
        calibrationTotal.Should().Be(expectedCalibrationTotal);
    }

    [TestMethod]
    [DataRow("day-07-sample.in.txt", 11387L)]
    [DataRow("day-07-test.in.txt", 249943041417600L)]
    public void PartTwo(string filePath, long expectedCalibrationTotal)
    {
        var calibrations = BridgeRepair.Parse(filePath);
        var calibrationTotal = BridgeRepair.TotalCalibrationResultWithConcatRecursive(calibrations);
        calibrationTotal.Should().Be(expectedCalibrationTotal);
    }
}
