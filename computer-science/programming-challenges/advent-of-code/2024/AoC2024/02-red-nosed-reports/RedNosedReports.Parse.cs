namespace AoC2024;

public static partial class RedNosedReports
{
    private static IEnumerable<IEnumerable<int>> GetReports(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;
        while ((line = inputReader.ReadLine()) != null)
        {
            yield return line.Split().Select(int.Parse);
        }
    }
}
