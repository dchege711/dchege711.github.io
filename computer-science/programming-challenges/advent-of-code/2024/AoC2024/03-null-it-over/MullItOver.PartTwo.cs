namespace AoC2024;

public static partial class MullItOver
{
    public static int PartTwo(string filePath)
    {
        return ParseCommands(filePath).Aggregate(
            new RunningSum(true, 0), 
            (res, cmd) => cmd switch {
                DoCommand doCmd => new(true, res.Sum),
                DontCommand dontCmd => new(false, res.Sum),
                MultiplyCommand multCmd => new(
                    res.Enabled,
                    res.Sum + (res.Enabled ? (multCmd.Num1 * multCmd.Num2) : 0)),
                _ => throw new ArgumentException($"Unrecognized type {cmd.GetType()}")
            },
            res => res.Sum);
    }

    private readonly record struct RunningSum(bool Enabled, int Sum);
}
