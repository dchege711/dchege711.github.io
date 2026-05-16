namespace AoC2024.WarehouseWoesDataTypes;

public enum Direction { Up, Right, Down, Left }

public record struct Coordinate(int R, int C)
{
    public override string ToString() => $"({R}, {C})";
}

public record struct Delta(int dR, int dC);

public enum CellType { Wall, Box, Free, BoxStart, BoxEnd }
