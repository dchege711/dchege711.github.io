namespace AoC2024.Tests;

[TestClass]
[DeploymentItem("data/scratchpad/day-11-sample.in.txt")]
[DeploymentItem("data/scratchpad/day-11-test.in.txt")]
public sealed class Day11PlutonianPebblesTests
{
    [TestMethod]
    public void Parse()
    {
        var stones = PlutonianPebbles.ReadStones("day-11-sample.in.txt");
        stones.Should().BeEquivalentTo([125, 17]);
    }

    public static IEnumerable<(ulong stone, IEnumerable<ulong> expectedStones)> TestBlinkData
    {
        get
        {
            return [
                (0, [1]),
                (123, [123 * 2024]),
                (1234, [12, 34]),
                (10, [1, 0]),
                (1000, [10, 0]),
                (100005, [100, 5]),
            ];
        }
    }

    [TestMethod]
    [DynamicData(nameof(TestBlinkData))]
    public void Blink(ulong stone, IEnumerable<ulong> expectedStones) =>
        PlutonianPebbles.Blink(stone).Should().BeEquivalentTo(expectedStones);

    [TestMethod]
    [DataRow("day-11-sample.in.txt", 25, 55312UL)]
    [DataRow("day-11-test.in.txt", 25, 235850UL)]
    [DataRow("day-11-test.in.txt", 75, 279903140844645UL)]
    public void Recursive(string fileName, int numBlinks, ulong expectedNumStones)
    {
        var stones = PlutonianPebbles.ReadStones(fileName);
        var numStones = PlutonianPebbles.NumStonesAfterBlinks(stones, numBlinks);
        numStones.Should().Be(expectedNumStones);
    }

    [TestMethod]
    [DataRow("day-11-sample.in.txt", 25, 55312UL)]
    [DataRow("day-11-test.in.txt", 25, 235850UL)]
    [DataRow("day-11-test.in.txt", 75, 279903140844645UL)]
    public void DynamicProgramming(string fileName, int numBlinks, ulong expectedNumStones)
    {
        var stones = PlutonianPebbles.ReadStones(fileName);
        var numStones = PlutonianPebbles.NumStonesAfterBlinksDP(stones, numBlinks);
        numStones.Should().Be(expectedNumStones);
    }
}
