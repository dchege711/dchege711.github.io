using System.Collections.Concurrent;

namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static ulong NumStonesAfterBlinks(IEnumerable<ulong> stones, int numBlinks)
    {
        var cache = new ConcurrentDictionary<(ulong, int), ulong>();
        return stones
            .AsParallel()
            .Select(stone => CountStonesAfterBlinks(stone, numBlinks, cache))
            .Aggregate(0UL, (acc, count) => acc + count);
    }

    public static IReadOnlyList<ulong> Blink(ulong stone)
    {
        if (stone == 0)
            return [1];
        
        var numDigits = GetNumberOfDigits(stone);
        if (numDigits % 2 == 0)
        {
            var newLength = numDigits / 2;
            var pow10 = (ulong)Math.Pow(10, newLength);
            var a = stone / pow10;
            return [a, stone - a * pow10];
        }

        return [stone * 2024UL];
    }

    private static ulong CountStonesAfterBlinks(
        ulong stone, int blinksRemaining, ConcurrentDictionary<(ulong, int), ulong> cache)
    {
        var subProblem = (stone, blinksRemaining);
        if (cache.TryGetValue(subProblem, out var numChildStones))
            return numChildStones;

        if (blinksRemaining <= 0) return 1;

        numChildStones = Blink(stone)
            .Select(childStone => CountStonesAfterBlinks(childStone, blinksRemaining - 1, cache))
            .Aggregate(0UL, (acc, count) => acc + count);
        
        cache[subProblem] = numChildStones;
        return numChildStones;
    }

    private static int GetNumberOfDigits(ulong n) => n switch
    {
        0 => 1,
        _ => (int)Math.Floor(Math.Log10(n)) + 1,
    };
}