using System.Diagnostics;
using AoC2024.WarehouseWoesDataTypes;
using ExhaustiveMatching;

namespace AoC2024;

public static class WarehouseWoesExtensions
{
    extension(Coordinate source)
    {
        public static Coordinate operator +(Coordinate coord, Delta delta) =>
            new(coord.R + delta.dR, coord.C + delta.dC);

        public static Coordinate operator -(Coordinate coord, Delta delta) =>
            new(coord.R - delta.dR, coord.C - delta.dC);
    }

    public static bool IsInBounds(this CellType[,] grid, Coordinate coordinate) =>
        coordinate.R >= 0 && coordinate.R < grid.GetLength(0)
        && coordinate.C >= 0 && coordinate.C < grid.GetLength(1);

    public static Delta ToDelta(this Direction direction) => direction switch
    {
        Direction.Down => new(1, 0),
        Direction.Up => new(-1, 0),
        Direction.Left => new(0, -1),
        Direction.Right => new(0, 1),
        _ => throw ExhaustiveMatch.Failed(direction)
    };

    public static bool IsLateral(this Direction direction) =>
        direction == Direction.Left || direction == Direction.Right;

    public static int SumBoxGpsCoordinates(this CellType[,] grid)
    {
        var sum = 0;
        for (int r = 0; r < grid.GetLength(0); r++)
        {
            for (int c = 0; c < grid.GetLength(1); c++)
            {
                var cellType = grid[r, c];
                switch (cellType)
                {
                    case CellType.BoxStart:
                    case CellType.Box:
                        sum += r * 100 + c;
                        break;
                    default:
                        continue;
                }
            }
        }
        return sum;
    }

    public static void Visualize(this CellType[,] grid, Coordinate robotPosition)
    {
        for (int r = 0; r < grid.GetLength(0); r++)
        {
            for (int c = 0; c < grid.GetLength(1); c++)
            {
                if (r == robotPosition.R && c == robotPosition.C)
                {
                    Debug.Write('@');
                    continue;
                }

                var val = grid[r, c] switch
                {
                    CellType.Wall => '#',
                    CellType.Box => 'O',
                    CellType.Free => '.',
                    CellType.BoxStart => '[',
                    CellType.BoxEnd => ']',
                    _ => throw ExhaustiveMatch.Failed(grid[r, c])
                };
                Debug.Write(val);
            }
            Debug.Write('\n');
        }
        Debug.WriteLine("");
    }
}
