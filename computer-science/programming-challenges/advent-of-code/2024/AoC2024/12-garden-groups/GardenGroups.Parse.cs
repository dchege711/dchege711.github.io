using System.Collections.Immutable;

namespace AoC2024;

public partial class GardenGroups
{
    private readonly Garden garden;

    public GardenGroups(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        var rowsBuilder = ImmutableList.CreateBuilder<char[]>();

        string? line;
        while ((line = inputReader.ReadLine()) != null)
            rowsBuilder.Add(line.ToCharArray());
        
        var rows = rowsBuilder.ToImmutable();
        int rowCount = rows.Count;
        int colCount = rows.First().Length;

        var plantMap = new char[rowCount, colCount];
        for (int r = 0; r < rowCount; r++)
            for (int c = 0; c < colCount; c++)
                plantMap[r, c] = rows[r][c];
        
        garden = new(plantMap, rowCount, colCount);
    }

    private record struct Garden(char[,] PlantMap, int RowCount, int ColCount);
}