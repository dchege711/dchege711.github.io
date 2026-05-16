namespace AoC2024;

public partial class GuardGallivant
{
    public int PartOne() => SimulateGuardMovement().VisitedPositions.Count();

    private (IEnumerable<Coordinate> VisitedPositions, bool IsTrapped) SimulateGuardMovement()
    {
        HashSet<Visit> visits = [StartingPosition];
        var orientation = StartingPosition.Orientation;
        var (dr, dc) = ToVector(orientation);
        var nr = StartingPosition.Coordinate.R + dr;
        var nc = StartingPosition.Coordinate.C + dc;
        var isTrapped = false;

        while (InBounds(nr, nc))
        {
            if (AreaMap.Obstacles.Contains(new(nr, nc)))
            {
                (nr, nc) = (nr - dr, nc - dc);
                orientation = TurnRight90Degrees(orientation);
                (dr, dc) = ToVector(orientation);
                continue;
            }

            Visit visit = new(new(nr, nc), orientation);

            if (visits.Contains(visit))
            {
                isTrapped = true;
                break;
            }
            
            visits.Add(visit);

            (nr, nc) = (nr + dr, nc + dc);
        }

        // VisualizeMapWithGuardMovement(visits);

        return (
            visits.GroupBy(visit => visit.Coordinate).Select(g => g.Key),
            isTrapped);
    }

    private static (int dr, int dc) ToVector(Orientation orientation) => orientation switch {
        Orientation.Up => (-1, 0),
        Orientation.Left => (0, -1),
        Orientation.Right => (0, 1),
        Orientation.Down => (1, 0),
        _ => throw new ArgumentException($"Unrecognized input: {orientation}")
    };

    private static Orientation TurnRight90Degrees(Orientation orientation) => orientation switch {
        Orientation.Up => Orientation.Right,
        Orientation.Right => Orientation.Down,
        Orientation.Down => Orientation.Left,
        Orientation.Left => Orientation.Up,
        _ => throw new ArgumentException("Invalid direction vector")
    };

    private bool InBounds(int r, int c) =>
        r >= 0 && r < AreaMap.RowCount && c >= 0 && c < AreaMap.ColCount;
}
