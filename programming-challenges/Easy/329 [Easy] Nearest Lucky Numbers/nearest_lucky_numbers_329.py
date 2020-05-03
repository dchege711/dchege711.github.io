"""
Finds the nearest lucky numbers. (https://en.wikipedia.org/wiki/Lucky_number)

Input: N (a positive integer)
    103
    225
    997

Output: previousLuckyNumber < n < nextLuckyNumber
    99 < 103 < 105
    223 < 225 < 231
    997 is a lucky number

"""

import sys
import os
import time
from pprint import pprint
sys.path.insert(0, os.environ["UTILITIES_PATH"])

from test_utilities import test_utilities

class node():
    def __init__(self, value):
        self.value = value
        self.next = None
        self.previous = None
        
    def has_next(self):
        return self.next is not None

class lucky_numbers():
    def __init__(self, max_n=None):
        assert isinstance(max_n, int) and max_n > 0, "max_n must be a positive int"
        start_time = time.time()
        self.queries = {}
        self.max_n = max_n
        self.root = node(1)
        
        # Create the linked list 
        previous_node = self.root
        for n in range(2, self.max_n + 1):
            new_node = node(n)
            previous_node.next = new_node
            new_node.previous = previous_node
            previous_node = new_node 
                   
        self._solve()
        end_time = time.time()
        print("Got the lucky numbers up to", "{0:,}".format(max_n), "in", "{0:.4f}".format(end_time - start_time), "sec")
        
        # Copy the lucky numbers into a list (so that we can use binary search)
        # The lucky numbers are already sorted...
        self.list_of_lucky_numbers = []
        current_node = self.root
        while current_node is not None:
            self.list_of_lucky_numbers.append(current_node.value)
            current_node = current_node.next
            
    def _solve(self):
        current_node = self.root
        while current_node is not None:
            removal_increment = current_node.value
            if removal_increment == 1:
                removal_increment = 2

            i = 1
            travelling_node = self.root
            while travelling_node is not None: 
                if (i % removal_increment == 0):
                    self._remove_node(travelling_node)
                travelling_node = travelling_node.next
                i += 1
                
            current_node = current_node.next
            
    def nearest_lucky_number(self, query):
        if query > self.max_n:
            return "The query should be at most {0:d}".format(self.max_n)
        else:
            return self._find_nearest_lucky_number(
                lo=0, hi=len(self.list_of_lucky_numbers)-1, query=query
            )
            
    def _find_nearest_lucky_number(self, lo=None, hi=None, query=None):
        mid = int((hi + lo) / 2)
        number_at_mid = self.list_of_lucky_numbers[mid]
        
        if number_at_mid == query:
            return "{0:d} is a lucky number".format(number_at_mid)
        elif lo > hi:
            return "{0:d} < {1:d} < {2:d}".format(
                self.list_of_lucky_numbers[hi],
                query, 
                self.list_of_lucky_numbers[lo]
            ) 
            
        elif number_at_mid > query:
            return self._find_nearest_lucky_number(
                lo=lo, hi=mid-1, query=query
            )
        else:
            return self._find_nearest_lucky_number(
                lo=mid+1, hi=hi, query=query
            ) 
            
            
    def _remove_node(self, node):
        if node is None:
            return
        previous_node = node.previous 
        next_node = node.next
        previous_node.next = next_node
        if next_node is not None:
            next_node.previous = previous_node     
    
    def __repr__(self):
        as_a_string = ""
        current_node = self.root
        while current_node is not None:
            if current_node.has_next():
                ending = " --> "
            else:
                ending = ""
            as_a_string = "".join([
                as_a_string, str(current_node.value), ending
            ])
            current_node = current_node.next
        return as_a_string

def main():
    test_object = test_utilities()
    with open(sys.argv[1], "r") as test_file:
        max_n = int(test_file.readline())
        lucky_num_finder = lucky_numbers(max_n=max_n)
        for line in test_file:
            items = line.split(",")
            query = int(items[0])
            expected_answer = items[1].strip()
            ans = lucky_num_finder.nearest_lucky_number(query)
            test_object.check(query, expected_answer, ans)
            
    print(test_object.summary())
    

if __name__ == "__main__":
    main()
