def get_stacks(num_stacks, boxes_to_be_stacked):
    # Fail early if possible
    total_height = 0
    for box in boxes_to_be_stacked:
        total_height += box
    if total_height % num_stacks != 0:
        return "(nothing)"
    
    height_per_stack = total_height / num_stacks
    boxes_to_be_stacked.sort().reverse()
    
    possible_initial_combos = get_possible_starting_combos(
        boxes_to_be_stacked, height_per_stack
    )
        
    
def get_possible_starting_combos(boxes_to_be_stacked, height_per_stack):
    initial_combos = set([])
    
    
def main():
    print(get_stacks(9, [2]))
    
if __name__ == "__main__":
    main()
