import sys 
sys.path.insert(0, "/Users/dchege711/utilities/")

from random import sample, randint
from math import log, ceil
from test_utilities import test_utilities

class primality_test():
    def __init__(self, method=None):
        self.available_methods = {
            "fermats_little_theorem"
        }
    
    def is_prime(self, possible_prime, required_accuracy, 
        method="fermats_little_theorem"):
        if method not in self.available_methods:
            raise ValueError(
                "%s isn't available. Try %s" %(method, self.available_methods)
            )
        if method == "fermats_little_theorem":
            return self._run_fermats_test(possible_prime, required_accuracy)
        
    def _run_fermats_test(self, possible_prime, required_accuracy=0.99):
        """
        Tests for primality using Fermat's Little Theorem.
        http://mathworld.wolfram.com/FermatsLittleTheorem.html
        
        Susceptible to Carmichael numbers: 
        http://mathworld.wolfram.com/CarmichaelNumber.html
        
        """
        # Because (1 - required_accuracy) = 2^(-num_samples_needed)
        num_samples = ceil(log(1/(1-required_accuracy), 2))
        if num_samples > possible_prime:
            num_samples = possible_prime
        
        try:
            sample_ints = sample(range(2, possible_prime), k=num_samples)
        except OverflowError:
            # I assume that if the number is large enough to create an overflow 
            # the chance that a random number gets picked twice is negligible.
            sample_ints = []
            for i in range(num_samples):
                sample_ints.append(randint(2, possible_prime))
        
        for sample_int in sample_ints:
            remainder = pow(sample_int, possible_prime, possible_prime)
            if remainder != sample_int:
                return False, 1 # If it fails we're sure the # is not prime
        
        accuracy = 1 - pow(2, -num_samples)
        return True, accuracy
    
def main():
    fermats_test = primality_test()
    test_utility = test_utilities()
    with open(sys.argv[1], "r") as test_file:
        for line in test_file:
            possible_prime, required_accuracy, expected_answer = line.split()
            possible_prime = int(possible_prime)
            required_accuracy = float(required_accuracy)
            answer = str(fermats_test.is_prime(possible_prime, required_accuracy)[0])
            test_utility.check(possible_prime, expected_answer, answer)
    print(test_utility.summary())
    
if __name__ == "__main__":
    main()
