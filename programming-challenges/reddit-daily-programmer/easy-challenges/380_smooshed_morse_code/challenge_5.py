"""challenge_5.py"""

from typing import List
from collections import defaultdict
import itertools

from _380_smooshed_morse_code import smorse, ALPHA_TO_MORSE
from morse_trie import MorseTrie
from smorse_utils import word_list

def get_coin_change(desired_amount: int, available_coins: List[int]) -> List[List[int]]:
    """
    Args:
        desired_amount: The amount of money that we wish to end up with

        available_coins: 
            The coin denominations. We assume that there is an infinite # of 
            each coin.
            
    Returns:
        A list of possible ways of getting `desired_amount` from `available_coins`
    """
    available_coins.sort()
    combinations_for_n = defaultdict(list)
    for coin in available_coins:
        combinations_for_n[coin] = [[coin]]

    for coin in available_coins:
        for amount in range(coin, desired_amount + 1):
            deficit = amount - coin
            for deficit_combo in combinations_for_n[deficit]:
                combinations_for_n[amount].append(
                    deficit_combo + [coin]
                )

    return combinations_for_n[desired_amount]

def morse_sequences_of_length_n(n: int) -> List[str]:
    """Yields: Unique morse code sequences of length `n`"""
    morse_characters_by_length = defaultdict(list)
    for morse_literal in ALPHA_TO_MORSE.values():
        morse_characters_by_length[len(morse_literal)].append(morse_literal)

    already_seen_sequences = set()
    for length_combo in get_coin_change(n, list(morse_characters_by_length.keys())):
        morse_literals_pool = []
        for length in length_combo:
            morse_literals_pool.append(morse_characters_by_length[length])

        for permutation in itertools.product(*morse_literals_pool):
            sequence = "".join(permutation)
            if sequence not in already_seen_sequences:
                already_seen_sequences.add(sequence)
                yield sequence

def solution() -> List[str]:
    """
    `--.---.---.--` is one of five 13-character sequences that does not appear 
    in the encoding of any word. Find the other four.

    Returns: `['---.---.-----', '--.---.------', '---.----.----', '---.---.---.-']`
    """
    valid_sequences = set()
    search_space = []
    for word in word_list:
        search_space.append(smorse(word))

    search_space = "+".join(search_space) # Any char that's not '-' or '.'

    for sequence in morse_sequences_of_length_n(13):
        if sequence not in search_space:
            valid_sequences.add(sequence)

    assert len(valid_sequences) == 5
    assert "--.---.---.--" in valid_sequences

    valid_sequences.remove("--.---.---.--")
    return list(valid_sequences)

if __name__ == "__main__":
    print(
        f"These 13-character sequences do not appear in any decoding {solution()}"
    )
