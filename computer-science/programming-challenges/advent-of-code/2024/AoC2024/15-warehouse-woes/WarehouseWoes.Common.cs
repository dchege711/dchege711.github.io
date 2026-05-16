using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public partial class WarehouseWoes
{
    public int Solve()
    {
        foreach (var direction in _directions)
            RobotPosition = Move(_grid, RobotPosition, direction);
        return _grid.SumBoxGpsCoordinates();
    }

    private static Coordinate Move(CellType[,] grid, Coordinate origin, Direction direction)
    {
        var cellsToShiftOver = GetCellsToShiftOver(grid, origin, direction);
        if (cellsToShiftOver.Count == 0)
            return origin;

        var delta = direction.ToDelta();
        foreach (var source in cellsToShiftOver.Reverse())
        {
            var target = source + delta;
            grid[target.R, target.C] = grid[source.R, source.C];
            grid[source.R, source.C] = CellType.Free;
        }

        return origin + delta;
    }

    private static IReadOnlyList<Coordinate> GetCellsToShiftOver(
        CellType[,] grid, Coordinate origin, Direction direction)
    {
        var delta = direction.ToDelta();

        IEnumerable<Coordinate> candidates = GetCellsAffectedBySingleMove(grid, origin, direction);
        List<Coordinate> cellsToShift = [];
        cellsToShift.Add(origin);
        while (candidates.All(grid.IsInBounds))
        {
            var cellTypes = candidates.Select(coord => grid[coord.R, coord.C]).ToArray();

            if (cellTypes.Any(ct => ct is CellType.Wall))
                return [];

            if (cellTypes.All(ct => ct is CellType.Free))
                return cellsToShift;

            cellsToShift.AddRange(candidates);
            candidates = candidates
                .SelectMany(coord => GetCellsAffectedBySingleMove(grid, coord, direction))
                .ToHashSet();
        }

        return [];
    }

    private static IEnumerable<Coordinate> GetCellsAffectedBySingleMove(
        CellType[,] grid, Coordinate origin, Direction direction)
    {
        var delta = direction.ToDelta();
        var target = origin + delta;

        if (direction.IsLateral())
        {
            yield return target;
            yield break;
        }

        if (!grid.IsInBounds(target))
        {
            yield return target;
            yield break;
        }

        var cellType = grid[target.R, target.C];
        switch (cellType)
        {
            case CellType.Wall:
            case CellType.Box:
                yield return target;
                yield break;
            case CellType.Free:
                yield break;
            case CellType.BoxStart:
                yield return target;
                yield return target + RightDelta;
                yield break;
            case CellType.BoxEnd:
                yield return target + LeftDelta;
                yield return target;
                yield break;
            default:
                throw ExhaustiveMatch.Failed(cellType);
        }
    }

    private static readonly Delta LeftDelta = new(0, -1);
    private static readonly Delta RightDelta = new(0, 1);
}
