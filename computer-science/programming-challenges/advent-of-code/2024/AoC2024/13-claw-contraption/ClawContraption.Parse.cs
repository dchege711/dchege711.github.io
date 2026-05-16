using System.Text.RegularExpressions;

namespace AoC2024;

public partial class ClawContraption
{
    public static IEnumerable<MachineConfig> Parse(string fileName)
    {
        using var inputReader = new StreamReader(fileName);
        while (inputReader.Peek() != -1)
        {
            var buttonA = ParseButton(inputReader.ReadLine());
            var buttonB = ParseButton(inputReader.ReadLine());
            var prize = ParsePrize(inputReader.ReadLine());
            inputReader.ReadLine();
            yield return new(buttonA, buttonB, prize);
        }
    }

    private static Button ParseButton(string? line)
    {
        if (line is null)
            throw new ArgumentException("line must not be null");

        Match match = ButtonLineRegex().Match(line);
        if (!match.Success)
            throw new ArgumentException($"{line} is not well formatted as a Button");
        
        long tokenCost = match.Groups["Type"].Value switch
        {
            "A" => 3,
            "B" => 1,
            _ => throw new ArgumentException($"{line} is missing button ID"),
        };
        var x = long.Parse(match.Groups["X"].Value);
        var y = long.Parse(match.Groups["Y"].Value);
        
        return new(new(x, y), tokenCost);
    }

    private static Vector ParsePrize(string? line)
    {
        if (line is null)
            throw new ArgumentException("line must not be null");

        Match match = PrizeLineRegex().Match(line);
        if (!match.Success)
            throw new ArgumentException($"{line} is not well formatted as a Button");
        
        return new(
            long.Parse(match.Groups["X"].Value),
            long.Parse(match.Groups["Y"].Value));
    }
    
    [GeneratedRegex(@"Button (?<Type>(A|B)): X\+(?<X>\d+), Y\+(?<Y>\d+)", default, 50)]
    private static partial Regex ButtonLineRegex();

    [GeneratedRegex(@"Prize: X=(?<X>\d+), Y=(?<Y>\d+)", default, 50)]
    private static partial Regex PrizeLineRegex();
}