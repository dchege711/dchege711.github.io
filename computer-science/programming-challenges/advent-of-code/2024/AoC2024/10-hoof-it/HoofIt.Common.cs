using System.Collections.Immutable;
using System.Diagnostics;

namespace AoC2024;

public partial class HoofIt
{
    private IEnumerable<TrailEndStats> GetTrailEndStats(Coordinate trailHead)
    {
        Queue<IReadOnlyList<Coordinate>> paths = [];
        paths.Enqueue([trailHead]);

        var hikingTrails = ImmutableList.CreateBuilder<IReadOnlyList<Coordinate>>();
        while (paths.Count > 0)
        {
            var path = paths.Dequeue();
            var current = path[path.Count - 1];

            if (topographicMap.Map[current.r, current.c] == TrailEndHeight)
            {
                Debug.Assert(path.Count == TrailLength);
                hikingTrails.Add(path);
                continue;
            }
            
            foreach (var next in PossibleMoves(current))
                paths.Enqueue([..path, next]);
        }

        return hikingTrails
            .GroupBy(trail => trail[trail.Count - 1]) // By the end of the trail.
            .Select(grouping => new TrailEndStats(grouping.Key, grouping.Count()));
    }

    private IEnumerable<Coordinate> PossibleMoves(Coordinate coordinate)
    {
        var (r, c) = coordinate;
        int targetHeight = topographicMap.Map[coordinate.r, coordinate.c] + 1;
        return Deltas
            .Select(d => new Coordinate(r + d.dr, c + d.dc))
            .Where(IsInBounds)
            .Where(newCoord => topographicMap.Map[newCoord.r, newCoord.c] == targetHeight);
    }

    private static readonly List<(int dr, int dc)> Deltas = [
        (-1, 0), (1, 0), (0, -1), (0, 1)];

    private bool IsInBounds(Coordinate coordinate)
    {
        int rowCount = topographicMap.Map.GetLength(0);
        int colCount = topographicMap.Map.GetLength(1);
        var (r, c) = coordinate;
        return r >= 0 && r < rowCount && c >= 0 && c < colCount;
    }

    private static readonly int TrailEndHeight = 9;
    private static readonly int TrailLength = 10;

    private record struct TrailEndStats(Coordinate TrailEnd, int DistinctPathsCount);
}