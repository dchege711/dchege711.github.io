"""challenge_3.py"""

from smorse_utils import word_list
from _380_smooshed_morse_code import smorse

def solution() -> str:
    """
    Call a word perfectly balanced if its code has the same number of dots as 
    dashes. `counterdemonstrations` is one of two 21-letter words that’s 
    perfectly balanced. Find the other one.
    
    Returns: `overcommercialization`
    """

    valid_balanced_words = set()
    for word in word_list:
        if len(word) != 21: continue
        
        smorsed_word = smorse(word)
        difference = 0
        for c in smorsed_word:
            if c == ".": difference += 1
            else: difference -= 1

        if difference == 0: valid_balanced_words.add(word)

    assert len(valid_balanced_words) == 2
    assert "counterdemonstrations" in valid_balanced_words
    valid_balanced_words.discard("counterdemonstrations")
    
    return valid_balanced_words.pop()

if __name__ == "__main__":
    print(f"The other balanced 21-letter word is {solution()}")
