from collections import defaultdict
from glob import glob

from typing import List

MORSE_TO_ALPHA = {
    '-': 't', '.-': 'a', '-...': 'b', '-.-.': 'c', '-..': 'd', '.': 'e', 
    '..-.': 'f', '--.': 'g', '....': 'h', '..': 'i', '.---': 'j', '-.-': 'k', 
    '.-..': 'l', '--': 'm', '-.': 'n', '---': 'o', '.--.': 'p', '--.-': 'q', 
    '.-.': 'r', '...': 's', '..-': 'u', '...-': 'v', '.--': 'w', '-..-': 'x', 
    '-.--': 'y', '--..': 'z'
}

LEN_LONGEST_MORSE_LITERAL = 0
LEN_VALID_SMORSE_PERMUTATION = 0
for k in MORSE_TO_ALPHA.keys():
    if len(k) > LEN_LONGEST_MORSE_LITERAL:
        LEN_LONGEST_MORSE_LITERAL = len(k)
    LEN_VALID_SMORSE_PERMUTATION += len(k)

def smorse_decode(smorse_text: str) -> List[str]:
    """
    Args:
        smorse_text: A smooshed morse encoding of a permutation of the alphabet

    Returns:
        A list of all alphabet permutations consistent with `smorse_text`
    """
    # Don't expend effort on a lost cause
    LEN_SMORSE_TEXT = len(smorse_text)
    if LEN_SMORSE_TEXT != LEN_VALID_SMORSE_PERMUTATION: return []

    # When you have DP, every problem is a memoization problem
    # I don't know how much memoization saves me, but analysis is as involved as 
    # solving the problem, so I'll dive right in and optimize later (if need be)
    
    permutations_ending_at_idx = defaultdict(list)
    for i in range(LEN_LONGEST_MORSE_LITERAL): 
        matching_alpha = MORSE_TO_ALPHA.get(smorse_text[:i+1])
        if matching_alpha is not None: 
            permutations_ending_at_idx[i] = [matching_alpha]

    for i in range(LEN_SMORSE_TEXT):
        # I'll bruteforce this for a bit and investigate if this needs a trie
        # Was fast enough for me, so no need for a trie
        max_j = min(i + LEN_LONGEST_MORSE_LITERAL, LEN_SMORSE_TEXT)
        for j in range(i, max_j):
            # Unfortunately, s[i:j] is n O(n) operation
            # Python Devs concluded that an O(1) implementation had 
            # pitfalls, e.g. uncollected garbage, type mismatches, etc. [1]
            # Rumor has it that Mozilla's JavaScript has O(1) splicing.
            #
            # [1]: https://mail.python.org/pipermail/python-dev/2008-May/079753.html
            #
            # Nonetheless, LEN_LONGEST_MORSE_LITERAL == 4. I don't copy too much
            matching_alpha = MORSE_TO_ALPHA.get(smorse_text[i:j+1])
            
            if matching_alpha is None: continue

            new_permutations = []
            for permutation in permutations_ending_at_idx[i-1]:
                if matching_alpha not in permutation:
                    new_permutations.append("".join((permutation, matching_alpha)))
            
            for permutation in new_permutations:
                permutations_ending_at_idx[j].append(permutation)

        # We never check permutations ending at `i-1` ever again
        if i - 1 in permutations_ending_at_idx:
            del permutations_ending_at_idx[i - 1]

    return permutations_ending_at_idx[LEN_SMORSE_TEXT-1]