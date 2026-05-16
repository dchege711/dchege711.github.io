using System.Collections.Immutable;

namespace AoC2024;

public partial class BridgeRepair
{
    public static long TotalCalibrationResultWithConcat(IEnumerable<CalibrationEquation> equations) =>
        TotalCalibrationResult(
            equations,
            ImmutableHashSet.Create([Operator.Add, Operator.Multiply, Operator.Concatenate]));
    
    private static long Concatenate(long a, long b)
    {
        var (factor, reducedB) = (10L, b);
        while ((reducedB /= 10) > 0)
            factor *= 10;
        return a * factor + b;
    }
}
