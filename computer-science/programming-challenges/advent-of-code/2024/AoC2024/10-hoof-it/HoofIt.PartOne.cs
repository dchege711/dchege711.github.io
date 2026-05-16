namespace AoC2024;

public partial class HoofIt
{    
    public int SumOfTrailHeadsScores() =>
        topographicMap
            .TrailHeads
            .Select(GetTrailEndStats)
            .Select(trailEnds => trailEnds.Count())
            .Sum();
}
