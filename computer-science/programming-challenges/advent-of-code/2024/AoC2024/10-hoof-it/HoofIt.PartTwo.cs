namespace AoC2024;

public partial class HoofIt
{
    public int SumOfTrailHeadsRatings() =>
        topographicMap
            .TrailHeads
            .Select(GetTrailEndStats)
            .SelectMany(trailEnds => trailEnds.Select(stats => stats.DistinctPathsCount))
            .Sum();
}
