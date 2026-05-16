namespace AoC2024;

public partial class ResonantCollinearity
{
    public int PartOne() => NumDistinctAntinodes(CollinearPairedAntinodes);

    private int NumDistinctAntinodes(
        Func<Coordinate, Coordinate, IEnumerable<Coordinate>> computeAntinodes)
    {
        HashSet<(Coordinate, Coordinate)> computedPairs = [];

        return antennasMap.AntennasByFrequency.Values
            .SelectMany(matchingAntennasCoordinates =>
                matchingAntennasCoordinates
                    .SelectMany((p1, i) =>
                        matchingAntennasCoordinates
                            .Skip(i + 1)
                            .Select(p2 => (p1, p2))
                            .Where(pair => !computedPairs.Contains(pair))
                            .Select(pair => {
                                computedPairs.Add(pair);
                                return computeAntinodes(pair.p1, pair.p2);
                            })))
                    .SelectMany(a => a)
            .Distinct()
            .Count();
    }

    private IEnumerable<Coordinate> CollinearPairedAntinodes(Coordinate p1, Coordinate p2)
    {
        var (dr, dc) = VectorP1ToP2(p1, p2);
        return new Coordinate[]
        {
            new(p2.R + dr, p2.C + dc),
            new(p1.R - dr, p1.C - dc)
        }.Where(InBounds);
    }

    private static (int dr, int dc) VectorP1ToP2(Coordinate p1, Coordinate p2) =>
        (p2.R - p1.R, p2.C - p1.C);

    private bool InBounds(Coordinate p) =>
        p.R >= 0 && p.R < antennasMap.RowCount
        && p.C >= 0 && p.C < antennasMap.ColCount;
}
