using System.Collections.Immutable;

namespace AoC2024;

public partial class GuardGallivant
{
    public enum Orientation { Up, Right, Down, Left }
    public readonly record struct Coordinate(int R, int C);
    public readonly record struct Visit(Coordinate Coordinate, Orientation Orientation);
    
    public readonly (int RowCount, int ColCount, HashSet<Coordinate> Obstacles) AreaMap;
    public readonly Visit StartingPosition;

    public GuardGallivant(string filePath)
    {
        using StreamReader inputReader = new(filePath);
    
        HashSet<Coordinate> obstacles = [];
        Visit? maybeStartingPosition = null;

        string? line;
        int r = 0;
        int colsCount = 0;
        while((line = inputReader.ReadLine()) is not null)
        {
            var cols = line.ToCharArray().Index().ToList();
            cols
                .ForEach(pair => {
                    Coordinate coordinate = new(r, pair.Index);
                    if (IsObstacle(pair.Item))
                    {
                        obstacles.Add(coordinate);
                        return;
                    }

                    if (ToOrientation(pair.Item) is not Orientation orientation)
                        return;
                    
                    if (maybeStartingPosition is not null)
                        throw new ArgumentException(
                            $"Multiple starting positions detected at {maybeStartingPosition} and {(r, pair.Index, orientation)}");
                    
                    maybeStartingPosition = new Visit(coordinate, orientation);
                });
            
            if (colsCount != 0 && cols.Count != colsCount)
                throw new ArgumentException($"Illegal grid. Found differing column counts: {colsCount} vs. {cols.Count}");
            
            colsCount = cols.Count;
            r++;
        }

        if (maybeStartingPosition is not Visit startingPosition)
            throw new ArgumentException("No starting position found");

        StartingPosition = startingPosition;
        AreaMap = new(r, colsCount, obstacles);
    }

    private static bool IsObstacle(char c) => c == '#';

    private static Orientation? ToOrientation(char c) => c switch
    {
        '^' => Orientation.Up,
        '<' => Orientation.Left,
        '>' => Orientation.Right,
        'v' => Orientation.Down,
        _ => null,
    };

    private char ToCharVisualization(Coordinate coordinate)
    {
        if (AreaMap.Obstacles.Contains(coordinate))
            return '#';
        
        if (coordinate == StartingPosition.Coordinate)
        {
            return StartingPosition.Orientation switch {
                Orientation.Up => '^',
                Orientation.Left => '<',
                Orientation.Right => '>',
                Orientation.Down => 'v',
                _ => throw new ArgumentException(
                    $"Unrecognized orientation {StartingPosition.Orientation}")
            };
        }

        return '.';
    }

    private void VisualizeMapWithGuardMovement(HashSet<Visit> visits)
    {
        HashSet<Coordinate> visitedCoordinates = [.. visits.Select(v => v.Coordinate)];
        for (int r = 0; r < AreaMap.RowCount; r++)
        {
            for (int c = 0; c < AreaMap.ColCount; c++)
            {
                Coordinate coordinate = new(r, c);
                char representation = visitedCoordinates.Contains(coordinate)
                    ? 'X'
                    : ToCharVisualization(coordinate);
                Console.Write(representation);
            }
            Console.WriteLine();
        }
    }
}
