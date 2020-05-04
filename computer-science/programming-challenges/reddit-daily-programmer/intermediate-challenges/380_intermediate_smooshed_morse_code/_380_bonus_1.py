"""_380_bonus_1.py"""

from timeit import timeit

from _380__smooshed_morse_code import get_matching_permutations

def process_bonus_input():
    """
    Run the `get_matching_permutations` routine on a 1,000 inputs. How fast is 
    the routine? Ans: 1,982 seconds (33 minutes, 2 seconds)
    """
    with open("bonus_data/1000_inputs.in", "r") as fp:
        for i, line in enumerate(fp):
            matches = get_matching_permutations(line.strip())
            assert matches, f"A match must be found for each line"

if __name__ == '__main__':
    # War Story: Tried optimizing `get_matching_permutations` without realizing 
    # `timeit` runs the code 1,000,000 times by default :-)
    print(
        timeit(
            "process_bonus_input()", 
            setup="from __main__ import process_bonus_input", number=1
        )
    )
