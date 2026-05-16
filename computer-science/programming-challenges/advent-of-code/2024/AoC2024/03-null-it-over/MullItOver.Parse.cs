using System.Text.RegularExpressions;

namespace AoC2024;

public static partial class MullItOver
{
    private static IEnumerable<ICommand> ParseCommands(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        string? line;
        while((line = inputReader.ReadLine()) != null)
        {
            var commands = InputLineRegex()
                .Matches(line)
                .Select(ParseCommand);
            
            foreach (var command in commands)
                yield return command;
        }
    }

    private static ICommand ParseCommand(Match m)
    {
        var groups = m.Groups;

        if (groups["do"].Success)
        {
            return new DoCommand();
        }
        else if (groups["dont"].Success)
        {
            return new DontCommand();
        }
        else if (groups["mul"].Success)
        {
            return new MultiplyCommand(
                int.Parse(groups["num1"].Value),
                int.Parse(groups["num2"].Value));
        }

        throw new ArgumentException($"Unrecognized match found {m}");
    }

    private interface ICommand;

    private record MultiplyCommand(int Num1, int Num2) : ICommand;
    private record DoCommand : ICommand;
    private record DontCommand : ICommand;

    [GeneratedRegex(@"(?<dont>don't\(\))|(?<do>do\(\))|(?<mul>mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\))")]
    private static partial Regex InputLineRegex();
}
