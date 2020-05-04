"""morse_trie.py"""

from _380_smooshed_morse_code import smorse
from typing import Iterable, List

class Node:
    """
    A node in the `MorseTrie`.

    Attributes:
        count (int) :
            The number of words whose smooshed morse code representation is the 
            path formed by the keys up to this node.

        children (dict):
            Nodes that lead to more smooshed morse code representation. As such 
            the morse code at this point is a prefix of all its children.

    """
    def __init__(self):
        self.count = 0
        self.children = {}

class MorseTrie:
    """
    A prefix tree whose paths encode morse code.

    Args:
        words: 
            An iterable of words (in the range [a...z]) to use as input for 
            building the morse trie

    """
    def __init__(self, words: Iterable):
        self._trie = Node()
        for word in words:
            self.add_word(word)

    def add_word(self, word) -> int:
        """
        Add the smooshed morse code representation of `word` to the trie.

        Returns: the number of words with that smooshed morse code representation.
        """
        self.add_morse_code(smorse(word))

    def add_morse_code(self, morse_text) -> int:
        """
        Add `morse_text` to the trie.

        Returns: the number of words with `morse_text` as their smooshed morse code.
        """
        node = self._trie
        for c in morse_text:
            if c not in node.children:
                node.children[c] = Node()
            node = node.children[c]
        node.count += 1
        return node.count

    def keys_with_count(self, count: int) -> List[str]:
        """
        Returns: 
            A list of all smooshed morse codes that have `count` number of words 
            mapping to them.
        """
        def __collect(keys_so_far: list, node: Node, matches: List[str]):
            if node.count == count: 
                matches.append("".join(keys_so_far))

            for key in node.children:
                matches = __collect(
                    keys_so_far + [key], node.children[key], matches
                )

            if keys_so_far: keys_so_far.pop()
            return matches

        return __collect([], self._trie, [])

    def word_exists(self, word) -> bool:
        """Returns: `True` if `words` smorse exists in the trie."""
        return self.morse_code_exists(smorse(word))

    def morse_code_exists(self, morse_code: str) -> bool:
        """Returns: `True` if `morse_code` exists in the trie."""
        def __find(node: Node, idx: int) -> bool:
            if idx >= len(morse_code): return True
            c = morse_code[idx]
            if c in node.children:
                return __find(node.children[c], idx + 1)
            return False

        return __find(self._trie, 0)

