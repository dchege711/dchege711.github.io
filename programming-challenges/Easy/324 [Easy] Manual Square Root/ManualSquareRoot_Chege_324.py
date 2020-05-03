'''
Write a program that outputs the highest number that is lower or equal than the
square root of the given number, with the given number of decimal fraction digits.

Sample Input

0 7720.17
1 7720.17
2 7720.17

Sample Output
87
87.8
87.86

Restrictions
Use the method described here:
https://medium.com/i-math/how-to-find-square-roots-by-hand-f3f7cadf94bb

'''

#_______________________________________________________________________________

import sys
import math

#_______________________________________________________________________________

def inbuiltSquareRoot(precisionAsString, numberAsString):
    '''
    Gives back the square root truncated to the specified precision.
    Use this to test inputs
    '''
    sqrt = math.sqrt(float(numberAsString))
    precision = int(precisionAsString)
    truncated = int(sqrt * (10 ** precision)) / (10 ** precision)
    return float('{:.{dp}f}'.format(truncated, dp = precision))

#_______________________________________________________________________________

def rootUsingRaphsonMethod(precisionAsString, numberAsString):
    '''
    Finds roots using the Newton-Raphson method.
    Function name ommited Newton because I'm a Liebniz guy.
    '''


#_______________________________________________________________________________

def manualSquareRoot(precisionAsString, numberAsString):
    '''
    Calculates the square root of number to the specified precision.
    Uses the manual square root method.
    '''

    numberAsString = padToMakeEven(numberAsString) # Modify number for manual method
    decimalIndex = numberAsString.find('.') # Helps us increase our precision counter
    originalLength = len(numberAsString) # Helps to det when we'll add zeros

    # Initialize variables to keep track of precision requirements
    precision = int(precisionAsString)
    currentPrecision = -1

    index = 0       # Keeps track of the number-pairs that we've considered
    remnant = 0     # The remainder after a long-division step
    soln = 0        # Holds the solution while it's being assembled
    divisor = 0     # Stores the previous divisor for the next step

    # Execute the manual square root algo till we get the required precision
    while currentPrecision != precision:

        # If we've exhausted the number, increase the ceiling by a factor of 100
        if index >= originalLength - 1:
            ceilingOfProduct = remnant * 100
        # Else, drop the next 2 numbers and set that as the ceiling
        else:
            ceilingOfProduct = joinNumbers(remnant, numberAsString[index:index+2])

        # The first step is special because the divisor gets multiplied by 2
        if index == 0:
            factor = 9
            # Find the largest product that fits within the ceiling
            while factor * factor > ceilingOfProduct:
                factor += -1
            # This factor will be the beginning of our solution
            soln = factor
            # Update the helper variables for the subsequent steps
            divisor = factor * 2
            remnant = ceilingOfProduct - (factor * factor)

        # All other subsequent steps can be generalized as follows:
        else:
            suffix = 9
            # Find the largest product that fits within the ceiling
            while joinNumbers(divisor, suffix) * suffix > ceilingOfProduct:
                suffix += -1
            # This is like concatenating the factor (suffix) to existing answer
            soln = joinNumbers(soln, suffix)
            # Update the helper variables for the subsequent steps
            remnant = ceilingOfProduct - joinNumbers(divisor, suffix) * suffix
            divisor = joinNumbers(divisor, suffix) + suffix

        # Increment the index by 2 since we want to look at pairs
        index += 2
        # Skip the decimal point. We're always bound to hit it.
        if index == decimalIndex:
            index += 1
        # Every step after the decimal point adds a new precision digit
        if index > decimalIndex:
            currentPrecision += 1

    # Include decimal points only if the user demands it
    if currentPrecision != 0:
        soln = soln / (10 ** currentPrecision)

    return soln

#_______________________________________________________________________________

def joinNumbers(prefixNumber, suffixNumber):
    '''
    Mimics the concatenation done by hand.
    '''
    suffixLength = len(str(suffixNumber))
    result = (int(prefixNumber) * (10 ** (suffixLength))) + int(suffixNumber)

    return result

#_______________________________________________________________________________

def padToMakeEven(numberAsString):
    '''
    Pads the number in case it has an odd number of digits
    '''
    decimalIndex = numberAsString.find('.')

    # If there's already a decimal point, don't add a new one
    if decimalIndex != -1:
        numberOfChars = len(numberAsString)
        digitsBeforePoint = decimalIndex
        # If the no. chars (with a d.p.) are even, the number of digits is not
        if (numberOfChars) % 2 == 0:
            # Pad at the front
            if digitsBeforePoint % 2 != 0:
                numberAsString = '0' + numberAsString
            # Pad at the back
            else:
                numberAsString = numberAsString + '0'

    # If there's no decimal point, add a new one and pad with 2 d.p.
    else:
        numberOfDigits = len(numberAsString)
        # Pad at the front, and then add the 2 d.p.
        if numberOfDigits % 2 != 0:
            numberAsString = '0' + numberAsString + '.00'
        # Pad at the back, and then add the 2 d.p.
        else:
            numberAsString = numberAsString + '.00'

    return numberAsString

#_______________________________________________________________________________

def main():
    '''
    Tests the functions and gives stats of how many tests were passed.
    '''
    with open(sys.argv[1], 'r') as inputNumbers:
        # Initialize helper variables to keep stats
        total = 0
        passed = 0
        # Read the precision and number from standard input
        for line in inputNumbers:
            line = line.split()
            # Compare the answers provided by various square root methods
            manualAns = manualSquareRoot(line[0], line[1])
            inbuiltAns = inbuiltSquareRoot(line[0], line[1])
            # Let me know which tests were missed
            if manualAns != inbuiltAns:
                print(numberAsString + " (" + precisionAsString + ") : ", end = "\t")
                print("Manual: " + str(manualAns) + " != Inbuilt: " + str(inbuiltAns))
            # If a test was passed, increase the success count
            else:
                passed += 1
            # Count the number of tests carried out
            total += 1

    # Print a summary of the test results
    print("\n_______________________\n")
    print("Passed", str(passed), "/", str(total), "tests!")
    print("\n_______________________\n")

#_______________________________________________________________________________

if __name__ == '__main__':
    main()

#_______________________________________________________________________________
