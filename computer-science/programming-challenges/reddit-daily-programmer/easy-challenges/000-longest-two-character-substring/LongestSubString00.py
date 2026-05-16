"""
Finds the longest sub-string that contains, at most, two characters.

Input:
An all lower-case string, e.g.
    abbccc
    abcabcabcabccc
    qwertyytrewq

Output:
Print the longest sub-string of the given string that contains, at most, two
unique characters.
If you find multiple sub-strings that match the description, print the last
sub-string (furthest to the right).
    bbccc
    bccc
    tyyt

"""

import sys

def getLongestSubstring(longString, n = 2):
    """
    Find the longest substring containing at most n unique characters.

    Param(s):

    longString (str)
        The string for which we're looking for a substring.

    n (int)
        The maximum number of unique characters allowed in the substring.

    """
    charIndexes = {}
    currentStart = 0
    bestStart = 0
    length = 0
    bestLength = 0
    currentStart = 0
    index = 0

    for currentChar in longString:
        print(currentChar, end = " ...\n")

        # Case 1: The character is already in the set.
        if currentChar in charIndexes.keys():
            # Note the most recent position in which the char occurs
            charIndexes[currentChar][1] = index
            length += 1
            # print("Incremented: ", charIndexes[currentChar])

        # Case 2: The character is not in the set
        else:
            numChars = len(charIndexes)

            # If necessary, make room for the new char
            if numChars == n:
                charToRemove = longString[currentStart]
                length = index - charIndexes[charToRemove][0]

                # Store the best result that we've encountered.
                if length >= bestLength:
                    bestLength = length
                    bestStart = currentStart

                # Start the new substring after the last occurence of the char
                # being removed.
                currentStart = charIndexes[charToRemove][1] + 1

                # Remove the old char
                charIndexes.pop(charToRemove, None)

            # Add the new character
            charIndexes[currentChar] = [index, index]

        # Store the best result that we've encountered.
        if length > bestLength:
            bestLength = length
            bestStart = currentStart

        print(charIndexes)
        index += 1

    return longString[bestStart: bestStart+bestLength+1]

def main():
    """
    Run the longest substring algorithm. Print the results to standard output.

    """
    count = 0
    successful = 0

    with open(sys.argv[1], 'r') as fileToRead:
        for line in fileToRead:
            items = line.split()
            substring = getLongestSubstring(items[0], n = 2)
            if substring == items[1]:
                successful += 1
            else:
                pass
                #print('{:20}'.format(items[0]), "Got:", '{:15}'.format(substring), "\tExpected:", items[1])
            count += 1
            break

    print("\n_________________________\n")
    print(successful, "/", count, "tests passed!")
    print("\n_________________________\n")

if __name__ == "__main__":
    main()
