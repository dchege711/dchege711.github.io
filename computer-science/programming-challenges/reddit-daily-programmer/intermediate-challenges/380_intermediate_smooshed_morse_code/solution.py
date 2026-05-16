from glob import glob
from timeit import timeit

from smooshed_morse_decoder import smorse_decode

def process_main_input():
    # Check all the provided inputs
    for input_file_path in glob("./data/*.in"):
        expected_output_file_path = input_file_path.replace(".in", ".ans")
        with open(input_file_path, "r") as input_fp:
            smorse_text = input_fp.readline().strip()
            with open(expected_output_file_path, "r") as ans_fp:
                valid_decoding = ans_fp.readline().strip()
                matches = set(smorse_decode(smorse_text))
                assert valid_decoding in matches, f"{valid_decoding} missing in {matches}"
                print(f"Passed {input_file_path}")

def process_bonus_input():
    """
    Run the `get_matching_permutations` routine on a 1,000 inputs. How fast is 
    the routine? Ans: 1,982 seconds (33 minutes, 2 seconds)
    """
    with open("bonus_data/1000_inputs.in", "r") as fp:
        for i, line in enumerate(fp):
            morse_input = line.strip()
            matches = smorse_decode(morse_input)
            assert matches, f"A match must be found for each line"

def time_routine(fn_name: str):
    # War Story: Tried optimizing `get_matching_permutations` without realizing 
    # `timeit` runs the code 1,000,000 times by default :-)
    print(
        timeit(
            f"{fn_name}()", 
            setup=f"from __main__ import {fn_name}", number=1
        )
    )

if __name__ == "__main__":
    time_routine("process_main_input")
    # time_routine("process_bonus_input")
