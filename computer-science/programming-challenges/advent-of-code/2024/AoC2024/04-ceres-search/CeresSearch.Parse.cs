namespace AoC2024;

public partial class CeresSearch
{
    private readonly char[,] grid;

    public CeresSearch(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        List<char[]> rows = [];
        string? line;
        while((line = inputReader.ReadLine()) != null)
            rows.Add(line.ToCharArray());

        int rowCount = rows.Count;
        int colCount = rows.First().Length;

        grid = new char[rowCount, colCount];
        for (int r = 0; r < rowCount; r++)
            for (int c = 0; c < colCount; c++)
                grid[r, c] = rows[r][c];
    }
}
