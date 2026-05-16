using System.Diagnostics;

namespace AoC2024;

public partial class GardenGroups
{
    public int ComputeTotalFencingPrice() =>
        ComputeRegions()
            .Sum(region => region.Area * region.Perimeter);
}