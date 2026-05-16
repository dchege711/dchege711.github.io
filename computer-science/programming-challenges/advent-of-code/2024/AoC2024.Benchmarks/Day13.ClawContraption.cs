using BenchmarkDotNet.Attributes;

namespace AoC2024.Benchmarks;

[MemoryDiagnoser]
public class Day13ClawContraption
{    
    [Benchmark]
    public long PartOne() =>
        ClawContraption.PartOne(BenchmarkUtils.GetResourcePath("day-13-test.in.txt"));
}