#!/usr/bin/env python

from non_abundant_sums import generate_abundant_nums

import cProfile
from pstats import SortKey

def sum_of_non_abundant_sums():
    """
    Solution for Solution for https://projecteuler.net/problem=23 inspired by
    Therryka's https://projecteuler.net/thread=23;page=8#411899.
    """
    # (28123, inf] can all be written as the sum of two abundant numbers.
    K = 28123

    abundant_nums = sorted(generate_abundant_nums(1, K - 1))
    abundant_nums_set = set(abundant_nums)

    ans = K * (K + 1) / 2
    for n in range(1, K + 1):
        # Test if there is an `a + b = n` where `a` and `b` are abundant.
        for a in abundant_nums:
            b = n - a

            # Abundant numbers need to be positive. Can break because
            # `abundant_nums` is sorted, and subsequent `b`s will be -ve.
            if b <= 0: break

            # We only need one such `a + b`. No need to search further.
            if b in abundant_nums_set:
                ans -= n
                break

    return ans

if __name__ == "__main__":
    cProfile.run("print(sum_of_non_abundant_sums())", sort=SortKey.TIME)
