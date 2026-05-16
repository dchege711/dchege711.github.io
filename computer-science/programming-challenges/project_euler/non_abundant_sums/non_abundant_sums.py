#!/usr/bin/env python

import cProfile
from pstats import SortKey

from itertools import combinations_with_replacement

from project_euler.amicable_numbers.amicable_numbers_deluxe import sum_of_proper_divisors

def generate_abundant_nums(lo, hi):
    """
    Generate the abundant numbers in the range [lo, hi], in sorted order.
    """
    for n in range(lo, hi + 1):
        if sum_of_proper_divisors(n) > n: yield n

def pairwise_sums(nums, N):
    """
    Given nums like [1, 4, 6], return unique sums like [2, 5, 7, 8, 10, 12].
    Sums that are greater than N are excluded.
    """
    sums = set([])
    for (a, b) in combinations_with_replacement(nums, 2):
        n = a + b
        if n > N: continue
        else: sums.add(n)

    return sums

def sum_of_non_abundant_sums():
    """
    Solution for https://projecteuler.net/problem=23
    """
    # (28123, inf] can all be written as the sum of two abundant numbers. Start
    # by assuming all numbers in [1, 28123] cannot be expressed as the sum of
    # two abundant numbers.
    N = 28123
    ans = N * (N + 1) / 2

    # Generate all numbers that can be expressed as the sum of two abundant
    # numbers. Stop at 28124 because beyond that is a futile exercise.
    abundant_nums = generate_abundant_nums(1, N - 1)
    for n in pairwise_sums(abundant_nums, N):
        ans -= n

    return ans

if __name__ == "__main__":
    cProfile.run("print(sum_of_non_abundant_sums())", sort=SortKey.TIME)
