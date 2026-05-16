#!/usr/bin/env python

from non_abundant_sums import generate_abundant_nums

import cProfile
from pstats import SortKey

def pairwise_sums(nums, K):
    """
    Given a sequence of numbers like [1, 4, 6], return unique sums like
    [2, 5, 7, 8, 10, 12]. Sums that are greater than N are excluded from the
    result.
    """
    # Ensure that the iterable is sorted. This also avoids a bug if `nums` is
    # a generator, as iterating over it more than once is incorrect.
    nums = sorted(nums)

    sums = set([])
    for r in nums:
        for c in nums:
            # Only consider the lower triangle of the grid. Fine to compare
            # values and not indices because `nums` is sorted and used in both
            # loops.
            if c > r: break

            s = r + c

            # Any other sum on this row will be greater than K
            if s > K: break

            sums.add(s)

    return sums

def sum_of_non_abundant_sums():
    """
    Solution for https://projecteuler.net/problem=23
    """
    # (28123, inf] can all be written as the sum of two abundant numbers. Start
    # by assuming all numbers in [1, 28123] cannot be expressed as the sum of
    # two abundant numbers.
    K = 28123
    ans = K * (K + 1) / 2

    # Generate all numbers that can be expressed as the sum of two abundant
    # numbers. Stop at 28124 because beyond that is a futile exercise.
    abundant_nums = generate_abundant_nums(1, K - 1)
    for n in pairwise_sums(abundant_nums, K):
        ans -= n

    return ans

if __name__ == "__main__":
    cProfile.run("print(sum_of_non_abundant_sums())", sort=SortKey.TIME)
