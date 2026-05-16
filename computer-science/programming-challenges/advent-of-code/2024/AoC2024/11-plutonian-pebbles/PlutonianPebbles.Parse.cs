namespace AoC2024;

public static partial class PlutonianPebbles
{
    public static IEnumerable<ulong> ReadStones(string filePath)
    {
        var line = File.ReadAllText(filePath).Trim();
        return line.Split().Select(ulong.Parse);
    }
}