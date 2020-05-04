import sys
from pprint import pprint

class bankers_algorithm():
    def __init__(self, initial_resources):
        self.initial_resources = initial_resources
        self.num_resources = len(initial_resources)
        self.processes = {}
        self.num_processes = 0
        
    def add_process(self, already_allocated, max_needed, process_name=None):
        len_already_allocated = len(already_allocated)
        len_max_needed = len(max_needed)
        assert len_already_allocated == len_max_needed, "{0:d} resources already allocated don't match {0:d} needed".format(
                len_already_allocated, len_max_needed
            )
        assert len_already_allocated == self.num_resources, "Process requires {0:d} resources but we have {0:d} available".format(
                len_already_allocated, self.num_resources
            )
        process_id = self.num_processes 
        self.num_processes += 1
        self.processes[process_id] = {}
        self.processes[process_id]["allocated"] = already_allocated
        self.processes[process_id]["max_needed"] = max_needed
        
    def order_processes(self):
        # Attempt starting with each of the processes in a bfs manner
        possible_arrangements = {}
        visited = set([])   # Fast lookup
        for process_id in self._neighbors(self.initial_resources, visited):
            visitation_order = [] # Keeps track of the order. Slow lookup
            possible_arrangements[process_id] = self._order_processes(
                process_id, self.initial_resources, visited, visitation_order
            )
            print("Done with the visits starting from", process_id, "\n")
        return possible_arrangements
    
    def _order_processes(self, process_id, available_resources, visited, visitation_order):
        print("Processing", process_id, "with", available_resources, end=" available resources, and ")
        
        # Carry out the process and free up its previously allocated resources
        newly_available_resources = [] 
        for i in range(self.num_resources):
            newly_available_resources.append(
                available_resources[i] + self.processes[process_id]["allocated"][i]
            )
            
        visited.add(process_id)
        visitation_order.append(process_id)
        
        feasible_neighbors = self._neighbors(newly_available_resources, visited)
        print("neighbors:", feasible_neighbors)
        for other_process_id in feasible_neighbors:
            # print("Exploring", other_process_id, "with", available_resources)
            other_available_resources, other_visited, other_visitation_order = self._order_processes(
                other_process_id, newly_available_resources, visited, visitation_order
            )
        
        print("Returning", available_resources, "after visiting", visitation_order, "from", process_id, "\n__________")
        return available_resources, visited, visitation_order
        
    def _neighbors(self, available_resources, visited):
        """
        Neighbors == processes that can be scheduled for the next time slot.
        
        """
        process_ids = []
        for process_id in self.processes:
            if process_id not in visited and self._feasible(process_id, available_resources):
                process_ids.append(process_id)
        return process_ids
    
    def _feasible(self, process_id, available_resources):
        """
        A process is feasible if it can be completed given the available resources
        
        """
        current_allocation = self.processes[process_id]["allocated"]
        max_needed = self.processes[process_id]["max_needed"]
        for i in range(self.num_resources):
            # If the process has an unfulfillable resource deficit, return False 
            if max_needed[i] - current_allocation[i] > available_resources[i]:
                return False
        return True
    
def main():    
    with open(sys.argv[1], "r") as test_file:
        num_processes = int(test_file.readline())
        
        initial_resources = test_file.readline().split()
        initial_resources = [int(x) for x in initial_resources]

        client = bankers_algorithm(initial_resources)
            
        for i in range(num_processes):
            process = test_file.readline().split(",")
            already_allocated = [int(x) for x in process[0].split()]
            max_needed = [int(x) for x in process[1].split()]
            client.add_process(already_allocated, max_needed)
    
    pprint(client.order_processes())
            
    
    
if __name__ == "__main__":
    main()
