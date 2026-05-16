namespace AoC2024.RestroomRedoubtDataTypes;

public static class RestroomRedoubtExtensions
{
    public static Vector WrapAround(this Vector self, int areaWidth, int areaHeight) =>
        new(WrapAround(self.X, areaWidth), WrapAround(self.Y, areaHeight));

    public static Vector MoveBy(this Vector self, Vector velocity, int duration) =>
        new(self.X + velocity.X * duration, self.Y + velocity.Y * duration);
    
    public static Robot Move(
        this Robot robot, int duration, int areaWidth, int areaHeight) => robot with {
            Position = robot.Position
                .MoveBy(robot.Velocity, duration)
                .WrapAround(areaWidth, areaHeight)};
    
    public static Quadrant GetQuadrant(this Vector v, int areaWidth, int areaHeight)
    {
        int horizontalBoundary = areaHeight / 2;
        int verticalBoundary = areaWidth / 2;

        if (v.X == verticalBoundary || v.Y == horizontalBoundary)
            return Quadrant.kBoundary;
        else if (v.X < verticalBoundary && v.Y < horizontalBoundary)
            return Quadrant.kTopLeft;
        else if (v.X > verticalBoundary && v.Y > horizontalBoundary)
            return Quadrant.kTopRight;
        else if (v.X > verticalBoundary && v.Y > horizontalBoundary)
            return Quadrant.kBottomRight;
        else
            return Quadrant.kBottomLeft;
    }

    private static int WrapAround(int a, int n) =>
        a >= 0 ? a % n : n - int.Abs(a) % n;
}
