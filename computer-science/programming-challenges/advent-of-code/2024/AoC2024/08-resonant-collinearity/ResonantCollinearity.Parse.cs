using System.Text.RegularExpressions;

namespace AoC2024;

public partial class ResonantCollinearity
{
    public readonly record struct Coordinate(int R, int C);
    public readonly record struct AntennasMap(
        int RowCount,
        int ColCount,
        IDictionary<char, List<Coordinate>> AntennasByFrequency);
    
    public readonly AntennasMap antennasMap;

    public ResonantCollinearity(string filePath)
    {
        Dictionary<char, List<Coordinate>> antennasByFrequency = [];

        string? line = null;
        int rowCount = 0;
        int colCount = 0;
        using var inputReader = new StreamReader(filePath);
        while ((line = inputReader.ReadLine()) != null)
        {
            foreach (Match match in antennaFrequencyRegex.Matches(line))
            {
                var frequency = match.Value[0];
                if (!antennasByFrequency.ContainsKey(frequency))
                    antennasByFrequency[frequency] = [];

                antennasByFrequency[frequency].Add(new (rowCount, match.Index));
            }

            if (colCount == 0)
            {
                colCount = line.Length;
            }
            else if (line.Length != colCount)
            {
                throw new ArgumentException(
                    $"Expected {colCount} columns but got {line.Length}");
            }

            rowCount++;
        }

        antennasMap = new(rowCount, colCount, antennasByFrequency);
    }

    private static Regex antennaFrequencyRegex = AntennaFrequencyRegex();

    [GeneratedRegex(@"[\w\d]")]
    private static partial Regex AntennaFrequencyRegex();  
}
