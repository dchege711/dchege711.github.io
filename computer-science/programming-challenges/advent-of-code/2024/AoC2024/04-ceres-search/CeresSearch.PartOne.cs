namespace AoC2024;

public partial class CeresSearch
{
    static readonly private List<(int, int)> possibleMoves = [
        (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)];

    static readonly private char[] targetString = ['X', 'M', 'A', 'S'];

    public int PartOne() => 
        Enumerable.Range(0, grid.GetLength(0))
            .SelectMany(r => Enumerable.Range(0, grid.GetLength(1))
                .Select(c => NumOccurrences(r, c)))
            .Sum();

    private int NumOccurrences(int r, int c)
    {
        if (grid[r, c] != targetString[0])
            return 0;
        
        return possibleMoves
            .Select(move => HasPatternStartingFromPosition(r, c, move.Item1, move.Item2))
            .Count(hasPattern => hasPattern);
    }

    private bool HasPatternStartingFromPosition(int r, int c, int dr, int dc)
    {
        return targetString.Index()
            .All(indexAndChar => {
                int nr = r + indexAndChar.Index * dr;
                int nc = c + indexAndChar.Index * dc;
                return nr >= 0
                    && nr < grid.GetLength(0)
                    && nc >= 0
                    && nc < grid.GetLength(1)
                    && grid[nr, nc] == indexAndChar.Item;
            });
    }
}
