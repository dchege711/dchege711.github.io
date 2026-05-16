namespace AoC2024;

public partial class GuardGallivant
{
    public int PartTwo() {
        var (visitedPositions, isTrapped) = SimulateGuardMovement();

        return visitedPositions
            .Where(position => position != StartingPosition.Coordinate)
            .Aggregate(
                isTrapped ? 1 : 0,
                (sum, coordinate) =>
                    sum + (IsGuardTrappedByAdditionalObstacle(coordinate) ? 1 : 0));
    }

    private bool IsGuardTrappedByAdditionalObstacle(Coordinate coordinate)
    {
        AreaMap.Obstacles.Add(coordinate);
        var isTrapped = SimulateGuardMovement().IsTrapped;
        AreaMap.Obstacles.Remove(coordinate);
        return isTrapped;
    }
}
