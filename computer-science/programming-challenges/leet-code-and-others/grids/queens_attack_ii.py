"""
https://www.hackerrank.com/challenges/queens-attack-2/problem
"""

from typing import Tuple, List


def queens_attack(n: int, _: int, r_q: int, c_q: int, obstacles: List[Tuple[int, int]]):
    """
    Given:
    `n`: an integer, the number of rows and columns in the board
    `k`: an integer, the number of obstacles on the board
    `r_q`: integer, the row number of the queen's position
    `c_q`: integer, the column number of the queen's position
    `obstacles`: a 2D array of integers where each element is has the row and
    column of an obstacle

    The coordinates are one-based, not zero-based. (1, 1) is the south-west
    corner of the board.

    Return the number of squares that the queen can attack.
    """
    south = r_q - 1
    north = n - south - 1
    west = c_q - 1
    east = n - west - 1
    north_east = min(north, east)
    south_east = min(south, east)
    south_west = min(south, west)
    north_west = min(north, west)

    def is_diagonal_to_queen(r, c):
        """Return True if (r, c) is on a diagonal to the queen."""
        return abs(r - r_q) == abs(c - c_q)

    for r, c in obstacles:
        assert not (
            r == r_q and c == c_q
        ), f"Obstacle ({r}, {c}) found on queen's position ({r_q}, {c_q})."
        if c == c_q:
            if r < r_q:
                south = min(south, r_q - r - 1)
            else:
                north = min(north, r - r_q - 1)
        elif r == r_q:
            if c < c_q:
                west = min(west, c_q - c - 1)
            else:
                east = min(east, c - c_q - 1)
        elif is_diagonal_to_queen(r, c):
            if r > r_q and c > c_q:
                north_east = min(north_east, r - r_q - 1)
            elif r > r_q and c < c_q:
                north_west = min(north_west, r - r_q - 1)
            elif r < r_q and c > c_q:
                south_east = min(south_east, r_q - r - 1)
            elif r < r_q and c < c_q:
                south_west = min(south_west, r_q - r - 1)

        limits = [
            north,
            north_east,
            east,
            south_east,
            south,
            south_west,
            west,
            north_west,
        ]
        assert all(
            limit >= 0 for limit in limits
        ), f"{limits} -ve after applying obstacle ({r}, {c}) to queen at ({r_q}, {c_q})"

    return sum(
        [north, north_east, east, south_east, south, south_west, west, north_west]
    )
