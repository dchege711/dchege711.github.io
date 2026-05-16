using AoC2024.WarehouseWoesDataTypes;

namespace AoC2024;

public partial class WarehouseWoes
{
    public Coordinate RobotPosition { get; private set; }

    private readonly CellType[,] _grid;
    private readonly IReadOnlyList<Direction> _directions;

    public WarehouseWoes(string filePath, bool isWideVersion)
    {
        var gridAndMoves = File.ReadAllText(filePath).Split("\n\n", 2);
        if (gridAndMoves.Length != 2)
            throw new ArgumentException($"{filePath} lacks either a grid or moves section");

        List<CellType[]> rows = [];
        Coordinate? maybeRobotPosition = null;
        foreach (var line in gridAndMoves[0].Split('\n'))
        {
            if (maybeRobotPosition is null)
            {
                var c = line.IndexOf('@');
                if (c != -1)
                    maybeRobotPosition = new(rows.Count, c * (isWideVersion ? 2 : 1));
            }

            rows.Add(line.SelectMany(c => ToCellTypes(c, isWideVersion)).ToArray());
        }

        if (maybeRobotPosition is not Coordinate robotPosition)
            throw new ArgumentException("Did not find starting position");

        RobotPosition = robotPosition;

        int R = rows.Count;
        if (R <= 0)
            throw new ArgumentException("Did not find a grid");

        int C = rows[0].Length;
        _grid = new CellType[R, C];
        for (int r = 0; r < R; r++)
            for (int c = 0; c < C; c++)
                _grid[r, c] = rows[r][c];

        List<Direction> directions = [];
        foreach (var line in gridAndMoves[1].Split('\n'))
            directions.AddRange(line.Select(ToDirection));
        _directions = directions;
    }

    public CellType[,] GetGridSnapshot()
    {
        int rows = _grid.GetLength(0);
        int cols = _grid.GetLength(1);
        CellType[,] copy = new CellType[rows, cols];
        Array.Copy(_grid, copy, _grid.Length);
        return copy;
    }

    private static Direction ToDirection(char c) => c switch
    {
        '<' => Direction.Left,
        '>' => Direction.Right,
        '^' => Direction.Up,
        'v' => Direction.Down,
        _ => throw new ArgumentException($"{c} cannot be parsed into a Direction.")
    };

    private static CellType[] ToCellTypes(char c, bool isWideVersion) =>
        isWideVersion ? ToCellTypesWide(c) : ToCellTypesNarrow(c);

    private static CellType[] ToCellTypesNarrow(char c) => c switch
    {
        '#' => NarrowWall,
        'O' => NarrowBox,
        '[' => NarrowBoxStart, // Support already expanded inputs for debugging.
        ']' => NarrowBoxEnd,   // Support already expanded inputs for debugging.
        '.' => NarrowFree,
        '@' => NarrowFree,
        _ => throw new ArgumentException($"{c} cannot be parsed into a CellType.")
    };

    private static CellType[] ToCellTypesWide(char c) => c switch
    {
        '#' => WideWall,
        'O' => WideBox,
        '.' => WideFree,
        '@' => WideFree,
        _ => throw new ArgumentException($"{c} cannot be parsed into a CellType.")
    };

    private static readonly CellType[] NarrowWall = [CellType.Wall];
    private static readonly CellType[] NarrowBox = [CellType.Box];
    private static readonly CellType[] NarrowFree = [CellType.Free];
    private static readonly CellType[] NarrowBoxStart = [CellType.BoxStart];
    private static readonly CellType[] NarrowBoxEnd = [CellType.BoxEnd];
    private static readonly CellType[] WideWall = [CellType.Wall, CellType.Wall];
    private static readonly CellType[] WideBox = [CellType.BoxStart, CellType.BoxEnd];
    private static readonly CellType[] WideFree = [CellType.Free, CellType.Free];
}
