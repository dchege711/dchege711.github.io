namespace AoC2024;

public partial class GardenGroups
{
    public int ComputeTotalDiscountedFencingPrice() =>
        ComputeRegions()
            .Sum(region => region.Area * region.VerticesCount);
}