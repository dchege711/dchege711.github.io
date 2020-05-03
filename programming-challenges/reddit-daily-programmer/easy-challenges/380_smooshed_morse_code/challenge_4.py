"""challenge_4.py"""

from _380_smooshed_morse_code import smorse
from smorse_utils import word_list

def solution():
    """
    `protectorate` is 12 letters long and encodes to `.--..-.----.-.-.----.-..--.`, 
    which is a palindrome. Find the only 13-letter word that encodes to a 
    palindrome.

    Returns: `intransigence`
    """
    def __is_palindrome(text):
        idx = 0
        max_idx = int(len(text) / 2)
        while idx < max_idx and text[idx] == text[-(idx + 1)]:
            idx += 1

        if idx == max_idx: return True
        return False

    assert __is_palindrome(smorse("protectorate"))

    valid_palindromes = set()
    for word in word_list:
        if len(word) != 13: continue
        if __is_palindrome(smorse(word)): valid_palindromes.add(word)

    assert len(valid_palindromes) == 1
    return valid_palindromes.pop()

if __name__ == "__main__":
    print(f"The other 13-letter palindrome is {solution()}")
    