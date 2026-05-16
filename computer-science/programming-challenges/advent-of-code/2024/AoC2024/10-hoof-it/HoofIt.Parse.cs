namespace AoC2024;

public partial class HoofIt
{
    public sealed record Coordinate(int r, int c);
    public sealed record TopographicMap(
        int[,] Map, IReadOnlyList<Coordinate> TrailHeads);

    public readonly TopographicMap topographicMap;

    public HoofIt(string filePath)
    {
        var lines = File.ReadLines(filePath).ToList();
    
        int rowCount = lines.Count;
        int colCount = lines.First().Length;
        int[,] map = new int[rowCount, colCount];
        List<Coordinate> trailHeads = [];

        for (int r = 0; r < rowCount; r++)
        {
            for (int c = 0; c < colCount; c++)
            {
                int height = lines[r][c] - '0';
                map[r, c] = height;
                if (height == TrailHeadHeight)
                    trailHeads.Add(new(r, c));
            }
        }

        topographicMap = new(map, trailHeads);
    }

    private readonly static int TrailHeadHeight = 0;
}
