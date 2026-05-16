"""
_380_smooshed_morse_code.py

Solve https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/

"""

from typing import List, Iterable
from collections import defaultdict
import os

"""A mapping of `[a...z]` to Morse code."""
ALPHA_TO_MORSE = {
    "a": ".-", "b": "-...", "c": "-.-.", "d": "-..", "e": ".",  
    "f": "..-.", "g": "--.", "h": "....", "i": "..", "j": ".---", 
    "k": "-.-", "l": ".-..", "m": "--", "n": "-.", "o": "---", 
    "p": ".--.", "q": "--.-", "r": ".-.", "s": "...", "t": "-", 
    "u": "..-", "v": "...-", "w": ".--", "x": "-..-", "y": "-.--", 
    "z": "--.."
}

def smorse(alpha_text: str) -> str:
    """
    Args:
        alpha_text: The string whose characters are in `[a...z]`

    Returns:
        The smooshed morse code representation of `alpha_text`

    Raises:
        ValueError if any character in `alpha_text` is not in `[a...z]`
    """
    smooshed_morse_text = [None] * len(alpha_text)
    for idx, c in enumerate(alpha_text):
        if c not in ALPHA_TO_MORSE:
            raise ValueError(
                f"{c} is not a character in a...z (case-sensitive)"
            )
        smooshed_morse_text[idx] = ALPHA_TO_MORSE[c]
    return "".join(smooshed_morse_text)

def main():
    assert smorse("sos") == "...---..."
    assert smorse("daily") == "-...-...-..-.--"
    assert smorse("programmer") == ".--..-.-----..-..-----..-."
    assert smorse("bits") == "-.....-..."
    assert smorse("three") == "-.....-..."
    
if __name__ == "__main__":
    main()
