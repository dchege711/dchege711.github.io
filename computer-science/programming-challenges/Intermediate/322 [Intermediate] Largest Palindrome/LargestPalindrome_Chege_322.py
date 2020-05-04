'''
Given an integer input n, this program prints the largest integer that
is a palindrome and has two factors both of string length n, e.g.

An input 2 gives an output of 9009
Explanation: 9009 has factors 99 and 91

1   9
2   9009
3   906609
4   99000099
5   9966006699

'''
#_______________________________________________________________________________

import sys
import time

#_______________________________________________________________________________

def getPalindromeDeluxe(n):
    '''
    Given an integer n, this method returns the greatest palindrome product
    formed by two n-digit integers
    '''
    # Brute force:
    # 99x99, 99x98, 99x97, ... , 98x98, 98x97, 98x96,...
    # Inefficiency: We are not looking at the largest first
    #
    # Inspection shows that there's an optimum traversal method
    #   ...
    #   ...     9216
    #   ...     9312    9409
    #   ...     9408    9506    9604
    #   ...     9504    9603    9702    9801
    #
    # If we make diagonal visits towards the lower-left corner, we'll
    # encounter the products in strictly decreasing order
    # That way, the first product that is a palindrome is our answer

    numberOfFactors = int(n)
    highestFactor = (10 ** numberOfFactors) - 1
    lowestFactor = 10 ** (numberOfFactors - 1)
    factorOne = highestFactor
    factorTwo = highestFactor

    # Look for the largest palindrome product.
    currentResult = -1
    while currentResult == -1:
        currentResult = visitLLNeighbors(lowestFactor, highestFactor, factorOne, factorTwo)
        factorOne, factorTwo = tryUpThenTryLeft(factorOne, factorTwo)

    return str(currentResult)

#_______________________________________________________________________________

def tryUpThenTryLeft(x, y):
    # Try giving the upper neighbor
    if (y - 1 <= x):
        return (x, y-1)
    # If there's no upper neighbor, return the left neighbor
    return (x-1, y)

#_______________________________________________________________________________

def visitLLNeighbors(lowestFactor, highestFactor, x, y):
    while (x >= lowestFactor and y <= highestFactor):
        product = x * y
        # print(str(x), "x", str(y), "=", str(product))
        if (isPalindrome(product)):
            return product
        x += -1
        y += 1
    return -1


#_______________________________________________________________________________

def isPalindrome(candidate):
    '''
    Given an integer, this returns True if the integer is a palindrome.
    Returns False otherwise
    '''
    candidate = str(candidate)
    backCursor = len(candidate) - 1
    frontCursor = 0

    while (frontCursor <= backCursor):
        if (candidate[frontCursor] != candidate[backCursor]):
            return False
        frontCursor += 1
        backCursor += -1
    return True

#_______________________________________________________________________________

def main():
    # Maintain counter variables to keep track of tests
    count = 0
    passed = 0

    with open(sys.argv[1], 'r') as inputFile:
        for line in inputFile:
            items = line.split()
            myPalindrome = getPalindromeDeluxe(items[0])
            # Note down any failed tests
            if myPalindrome != items[1]:
                print(str(myPalindrome), "should be", items[1])
            else:
                passed += 1
            count += 1

    # Print a summary of the tests
    print("____________________\n")
    print(str(passed), "/", str(count), "tests passed!")
    print("____________________\n")

#_______________________________________________________________________________

def printListOfPalindromes(N):
    outputFile = open('palindromes.txt', 'w')
    n = 2
    while n <= N:
        startTime = time.time()
        palindrome = getPalindromeDeluxe(n)
        stopTime = time.time()
        outputFile.write(str(n) + "\t" + palindrome + "\t" + str(stopTime-startTime) + "\n")
        print(str(n), "\t", palindrome, "\t", str(stopTime-startTime))
        n += 1

if __name__ == '__main__':
    main()
    # printListOfPalindromes(10)
