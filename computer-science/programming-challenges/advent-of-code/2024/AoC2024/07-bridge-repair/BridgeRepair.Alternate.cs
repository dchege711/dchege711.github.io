using System.Collections.Immutable;

namespace AoC2024;

public partial class BridgeRepair
{
    public static long TotalCalibrationResultRecursive(
        IEnumerable<CalibrationEquation> equations) =>
        TotalCalibrationResulRecursive(
            equations,
            ImmutableHashSet.Create([Operator.Add, Operator.Multiply]));

    public static long TotalCalibrationResultWithConcatRecursive(
        IEnumerable<CalibrationEquation> equations) =>
        TotalCalibrationResulRecursive(
            equations,
            ImmutableHashSet.Create([Operator.Add, Operator.Multiply, Operator.Concatenate]));

    private static long TotalCalibrationResulRecursive(
        IEnumerable<CalibrationEquation> equations, ImmutableHashSet<Operator> operators) =>
        equations
            .AsParallel()
            .Where(eq => IsValid(eq, operators, eq.Operands.First(), 0))
            .Select(eq => eq.Result)
            .Sum();

    private static bool IsValid(
        CalibrationEquation equation,
        ImmutableHashSet<Operator> operators,
        long accumulatedResult,
        int numOperatorsUsed)
    {
        if (accumulatedResult == equation.Result && numOperatorsUsed == equation.Operands.Count - 1)
            return true;
        
        if (accumulatedResult > equation.Result)
            return false;
        
        if (numOperatorsUsed >= equation.Operands.Count - 1)
            return false;

        numOperatorsUsed += 1;
        var operand = equation.Operands[numOperatorsUsed];

        return operators
            .Any(@operator =>
                IsValid(
                    equation,
                    operators,
                    Apply(accumulatedResult, @operator, operand),
                    numOperatorsUsed));
    }
}
