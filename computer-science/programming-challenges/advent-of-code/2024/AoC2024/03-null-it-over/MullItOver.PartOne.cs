namespace AoC2024;

public static partial class MullItOver
{
    public static int PartOne(string filePath)
    {
        return ParseCommands(filePath)
            .OfType<MultiplyCommand>()
            .Select(cmd => cmd.Num1 * cmd.Num2)
            .Sum();
    }
}
