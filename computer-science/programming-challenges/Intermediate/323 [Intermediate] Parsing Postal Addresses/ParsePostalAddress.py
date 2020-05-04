'''
Given an address as a multi-line block, emit a labelled data structure
representing the address.

Sample Input

Tudor City Greens
24-38 Tudor City Pl
New York, NY
10017
USA

Sample Output

business=Tudor City Greens
address=24-38
street=Tudor City Pl
city=New York
state=NY
postal_code=10017
country=USA

The program shoud work for other forms of addresses...

Docks               Hotel Hans Egede    Alex Bergman            Dr KS Krishnan Marg
633 3rd Ave         Aqqusinersuaq       Wilhelmgalerie          South Patel Nagar
New York, NY        Nuuk 3900           Platz der Einheit 14    Pusa
10017               Greenland           14467 Potsdam           New Delhi, Delhi
USA                 +299 32 42 22       Germany                 110012
(212) 986-8080                          +49 331 200900          India

'''

#_______________________________________________________________________________

import sys
import re

#_______________________________________________________________________________

# The line before phone is always the country
phonePlus = r'\+(\d* *)*'
phoneUS = r'\(*\d*\)(\d* *-*)*'
# Then extract the town and city
townAndCity = r'.*,.*'

#_______________________________________________________________________________

def readInput():
    with open(sys.argv[1], 'r') as myAddresses:
        blockAddress = []
        for line in myAddresses:
            # In the text file '***' delimits the start of another address
            if line == '***':
                parseAddress(blockAddress)
                blockAddress = []

            else:
                blockAddress.append(line)

#_______________________________________________________________________________

if __name__ == '__main__':
    readInput()
#_______________________________________________________________________________
