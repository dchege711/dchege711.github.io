"""challenge_2.py"""

from smorse_utils import word_list
from _380_smooshed_morse_code import smorse

def solution() -> str:
    """
    `autotomous` encodes to `.-..--------------..-...`, which has 14 dashes in 
    a row. Find the only word that has 15 dashes in a row.

    Returns: `bottommost` which encodes to `-...---------------...-`
    """
    for word in word_list:
        smorsed_word = smorse(word)
        if "---------------" in smorsed_word:
            return word
    return None

if __name__ == "__main__":
    word = solution()
    print(f"{word} -> {smorse(word)} has 15 dashes in a row")



# Challenge: `--.---.---.--` is one of five 13-character sequences that 
# does not appear in the encoding of any word. Find the other four.
# morse_trie = MorseTrie(alpha_words())
# for idx, word in enumerate(alpha_words()):
#     assert morse_trie.word_exists(word)
#     if idx > 10: break

def get_char_sequences_of_size_n(n: int) -> Iterable[str]:
    morse_literals_lengths = {}
    for morse_literal in ALPHA_TO_MORSE.values():
        length = len(morse_literal) 
        if length not in morse_literals_lengths:
            morse_literals_lengths[length] = []
        morse_literals_lengths[length].append(morse_literal)

    available_lengths = list(morse_literals_lengths.keys())


get_char_sequences_of_size_n(13)