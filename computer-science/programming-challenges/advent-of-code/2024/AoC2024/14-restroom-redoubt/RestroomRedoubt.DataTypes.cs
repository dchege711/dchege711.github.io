namespace AoC2024.RestroomRedoubtDataTypes;

public record struct Vector(int X, int Y);

public record struct Robot(Vector Position, Vector Velocity);

public enum Quadrant { kTopLeft, kTopRight, kBottomRight, kBottomLeft, kBoundary };
