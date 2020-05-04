"""
Generates the Baum-Sweet Sequence.

"""

import sys
import os
sys.path.insert(0, os.environ["UTILITIES_PATH"])
from test_utilities import test_utilities


def baum_sweet_number(n):
	num_in_binary = format(n, 'b')
	zero_in_binary = format(0, 'b')

	# Edge case for n == 0
	if num_in_binary == zero_in_binary:
		return 1

	# Create a flag that will be toggled by each zero
	have_seen_odd_consecutive_zeros = False
	for digit in num_in_binary:
		# If the digit is a zero, flip the flag
		if digit == zero_in_binary:
			have_seen_odd_consecutive_zeros = not have_seen_odd_consecutive_zeros
		# If the digit is a one, and we've seen a consecutive # of zeros,
		# break from the loop
		elif have_seen_odd_consecutive_zeros:
			break
	
	# Return the Baum-Sweet number
	if have_seen_odd_consecutive_zeros:
		return 0
	else:
		return 1


def main():
	test = test_utilities()
	with open(sys.argv[1], 'r') as input_file:
		for line in input_file:
			items = line.split()
			number = int(items[0])
			baum_number = int(items[1])
			test.check(number, baum_number, baum_sweet_number(number))

	print(test.summary())

if __name__ == "__main__":
	main()
