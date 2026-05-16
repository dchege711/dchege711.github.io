namespace AoC2024;

public partial class ResonantCollinearity
{
    public int PartTwo() => NumDistinctAntinodes(CollinearAntinodes);

    private IEnumerable<Coordinate> CollinearAntinodes(Coordinate p1, Coordinate p2)
    {
        var (dr, dc) = VectorP1ToP2(p1, p2);
        
        var antinode = p1;
        while (InBounds(antinode))
        {
            yield return antinode;
            antinode = new(antinode.R - dr, antinode.C - dc);
        }

        antinode = new(p1.R + dr, p1.C + dc);
        while (InBounds(antinode))
        {
            yield return antinode;
            antinode = new(antinode.R + dr, antinode.C + dc);
        }
    }
}
