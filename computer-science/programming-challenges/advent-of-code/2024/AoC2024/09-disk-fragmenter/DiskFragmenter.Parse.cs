namespace AoC2024;

public partial class DiskFragmenter
{
    public static IEnumerable<int> Parse(string filePath)
    {
        int zeroAsciiVal = (int)'0';
        int newLineAsciiVal = (int)'\n';

        using var inputReader = new StreamReader(filePath);
        int c;
        while((c = inputReader.Read()) != newLineAsciiVal)
            yield return c - zeroAsciiVal;
    }
}
