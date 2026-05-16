namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static ulong NumStonesAfterBlinksDP(IEnumerable<ulong> stones, int numBlinks)
    {
        var stoneCounts = stones.GroupBy(s => s).ToDictionary(g => g.Key, g => (ulong)g.Count());

        for (int blink = 0; blink < numBlinks; blink++)
        {
            var nextCounts = new Dictionary<ulong, ulong>();
            foreach (var (stone, count) in stoneCounts)
            {
                foreach (var newStone in Blink(stone))
                {
                    nextCounts[newStone] = nextCounts.GetValueOrDefault(newStone) + count;
                }
            }
            stoneCounts = nextCounts;
        }

        return stoneCounts.Values.Aggregate(0UL, (acc, count) => acc + count);
    }
}