namespace AoC2024;

using Vector = ClawContraption.Vector;
using Button = ClawContraption.Button;
using MachineConfig = ClawContraption.MachineConfig;
using DirectedEdge = ClawContraption.DirectedEdge;

public partial class ClawContraption
{
    public record struct Vector(long X, long Y);

    public record struct Button(Vector Delta, long TokenCost);

    public record struct MachineConfig(Button A, Button B, Vector Prize);

    public record struct DirectedEdge(Vector From, Vector To, long Cost);
}

public static class ClawContraptionExtensions
{
    public static Vector Move(this Vector vector, Button button) =>
        new(vector.X + button.Delta.X, vector.Y + button.Delta.Y);
    
    public static bool IsBeyondPrizeLocation(this Vector vector, MachineConfig config) =>
        vector.X > config.Prize.X || vector.Y > config.Prize.Y;
    
    public static IEnumerable<DirectedEdge> GetEdges(this Vector from, MachineConfig config)
    {
        var to = from.Move(config.A);
        if (!to.IsBeyondPrizeLocation(config))
            yield return new(from, to, config.A.TokenCost);
        
        to = from.Move(config.B);
        if (!to.IsBeyondPrizeLocation(config))
            yield return new(from, to, config.B.TokenCost);
    }
}