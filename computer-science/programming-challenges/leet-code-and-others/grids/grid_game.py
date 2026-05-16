"""
https://leetcode.com/problems/grid-game/description/
"""

from typing import List


def grid_game(grid: List[List[int]]) -> int:
    """
    Given a `2 x n` grid, find the maximum score obtainable by a robot `r_2`
    if there is a robot `r_1` that takes a first pass at the grid and zeroes
    out one path from (0, 0) to (1, n-1). Each robot starts from (0, 0) and can
    only move right/down.
    """
    assert len(grid) == 2, f"Expected 2 rows, found {len(grid)}"

    n = len(grid[0])
    assert all(len(row) == n for row in grid), f"All rows must have length {n}"

    # Set the initial candidates
    top_sum = sum(grid[0][1:])
    bottom_sum = 0
    best_r2_score = top_sum

    # Evaluate the rest of the N-1 paths and minimize `best_r2_score`.
    for drop_idx in range(1, n):
        top_sum -= grid[0][drop_idx]
        bottom_sum += grid[1][drop_idx - 1]
        best_r2_score = min(best_r2_score, max(top_sum, bottom_sum))

    return best_r2_score
