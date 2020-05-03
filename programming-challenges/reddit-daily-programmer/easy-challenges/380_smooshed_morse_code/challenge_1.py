"""challenge_1.py"""

from typing import List

from morse_trie import MorseTrie
from smorse_utils import word_list

def solution() -> str:
    """
    The sequence `-...-....-.--.` is the code for four different words 
    (`needing`, `nervate`, `niding`, `tiling`). Find the only sequence that’s 
    the code for 13 different words.

    Returns: `-....--....`
    """
    morse_trie = MorseTrie(["needing", "nervate", "niding", "tiling"])
    matching_codes = morse_trie.keys_with_count(4)
    assert len(matching_codes) == 1
    assert matching_codes[0] == "-...-....-.--."

    morse_trie = MorseTrie(word_list)
    return morse_trie.keys_with_count(13)[0]

if __name__ == "__main__":
    print(
        f"{solution()} encodes 13 different words"
    )
