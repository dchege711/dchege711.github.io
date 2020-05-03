#include <iostream>
#include <fstream>
#include <cmath>

/**
 * Get the fuel required to launch a module that has mass `module_mass`. The 
 * fuel required is the mass, divided by three, rounded down, and subtracted 2.
 * 
 * However, the fuel, also requires fuel, unless the required fuel would be zero.
 * Treat the amount of fuel as equal to the mass of that fuel.
 */
int fuel_units_needed_to_launch(int module_mass) {
    int fuel_needed = std::max(0, (int)(std::floor(module_mass / 3.0) - 2));
    if (fuel_needed > 0) {
        return fuel_needed + fuel_units_needed_to_launch(fuel_needed);
    } else {
        return fuel_needed;
    }
}

/**
 * For a single source program like this one, `$ make tyranny_of_the_rocket_equation`
 * is good enough to build the thing. 
 * https://stackoverflow.com/questions/221185/how-to-compile-and-run-c-c-in-a-unix-console-mac-terminal
 */ 
int main() {
    std::ifstream input_file("input01.txt");
    if (!input_file.is_open()) {
        std::cerr << "Could not open file.";
        return 0;
    }

    int module_mass;
    int total_fuel_needed = 0;

    while(input_file >> module_mass) {
        total_fuel_needed += fuel_units_needed_to_launch(module_mass);
    }

    input_file.close();
    std::cout << total_fuel_needed << " fuel units are needed." << std::endl;
}