#!/usr/bin/env python

def sum_of_proper_divisors(n):
    if n == 0: return 0
    orig_n = n
    s, factor = 1, 2
    while factor * factor <= n and n > 1: # We only need to check up to sqrt(n)
        multiplicity = 0
        while n % factor == 0:
            n /= factor
            multiplicity += 1

        if multiplicity > 0:
            s *= (factor ** (multiplicity + 1) - 1) / (factor - 1)

        factor = 3 if factor == 2 else factor + 2

    # Account for any remaining prime factor greater than sqrt(n). There will be
    # at most one such factor. (n^2 - 1) / (n-1) simplifies to (n + 1).
    if n > 1: s *= (n + 1)

    return s - orig_n

def is_amicable_number(n):
    s = sum_of_proper_divisors(n)
    if s == n: return False
    return sum_of_proper_divisors(s) == n

def sum_of_amicable_numbers(n):
    return sum(filter(is_amicable_number, range(1, n)))

if __name__ == "__main__":
    print(sum_of_amicable_numbers(10000))
