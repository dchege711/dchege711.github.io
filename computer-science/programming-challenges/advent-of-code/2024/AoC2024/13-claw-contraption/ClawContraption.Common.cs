namespace AoC2024;

public partial class ClawContraption
{
    private static long GetMinimumCost(MachineConfig config) =>
        GetMinimumCostWithAnalyticalSolution(config, out var minCost)
            ? minCost
            : GetMinimumCostWithShortestPaths(config);

    private static bool GetMinimumCostWithAnalyticalSolution(MachineConfig config, out long minCost)
    {
        minCost = long.MaxValue;

        // Solve for A by eliminating B from the system of equations.
        //
        // dX_A * A + dX_B * B = X  |  dY_B (dX_A * A + dX_B * B = X)
        // dY_A * A + dY_B * B = Y  |  dX_B (dY_A * A + dY_B * B = Y)
        //
        // (dY_B * dX_A - dX_B * dY_A) * A = dY_B * X - dX_B * Y
        long aCoeff = config.B.Delta.Y * config.A.Delta.X - config.B.Delta.X * config.A.Delta.Y;
        if (aCoeff == 0)
            return false;
        
        long rhs = config.B.Delta.Y * config.Prize.X - config.B.Delta.X * config.Prize.Y;

        double aMoveCount = rhs * 1.0 / aCoeff;
        if (!double.IsInteger(aMoveCount))
            return true;
        
        // Solve for B by plugging in A back into one of the equations.
        //
        // dX_A * A + dX_B * B = X
        // dX_B * B = X - dX_A * A
        double bMoveCount = (config.Prize.X - (config.A.Delta.X * aMoveCount)) / config.B.Delta.X;
        if (!double.IsInteger(bMoveCount))
            return true;
        
        minCost = double.ConvertToInteger<long>(aMoveCount * config.A.TokenCost + bMoveCount * config.B.TokenCost);
        return true;
    }

    private static long GetMinimumCostWithShortestPaths(MachineConfig config)
    {
        var pq = new PQWithReplace();
        pq.Upsert(new(0, 0), long.MaxValue, 0);
        var distTo = new Dictionary<Vector, long>([new(new(0L, 0L), 0L)]);

        while (!pq.IsEmpty())
        {
            var v = pq.Dequeue();
            if (v == config.Prize)
                break;

            foreach (var edge in v.GetEdges(config))
            {
                var prevCost = GetCost(distTo, edge.To);
                var currCost = GetCost(distTo, v) + edge.Cost;
                if (prevCost > currCost)
                {
                    distTo[edge.To] = currCost;
                    pq.Upsert(edge.To, prevCost, currCost);
                }
            }
        }

        return GetCost(distTo, config.Prize);
    }

    private static long GetCost(Dictionary<Vector, long> distTo, Vector v)
    {
        if (distTo.TryGetValue(v, out var cost))
            return cost;
        
        return long.MaxValue;
    }
}

public class PQWithReplace
{
    private SortedSet<long> _knownCosts = [];

    private Dictionary<long, HashSet<ClawContraption.Vector>> _vectors = [];

    public bool IsEmpty() => _knownCosts.Count == 0;

    public void Upsert(ClawContraption.Vector v, long prevCost, long cost)
    {
        // Remove any previous entry.
        if (prevCost != long.MaxValue)
        {
            var prevSet = _vectors[prevCost];
            prevSet.Remove(v);

            if (prevSet.Count == 0)
            {
                _vectors.Remove(prevCost);
                _knownCosts.Remove(prevCost);
            }
        }

        // Insert new entry.
        if (!_vectors.ContainsKey(cost))
            _vectors[cost] = [];

        _vectors[cost].Add(v);
        _knownCosts.Add(cost);
    }
    
    public ClawContraption.Vector Dequeue() {
        var cost = _knownCosts.First();
        var set = _vectors[cost];
        var vector = set.First();
        set.Remove(vector);

        if (set.Count == 0)
        {
            _vectors.Remove(cost);
            _knownCosts.Remove(cost);
        }

        return vector;
    }
}