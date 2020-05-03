use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

/// Returns the amount of fuel required to launch a module of mass `mass`. 
fn fuel_needed_for_mass(mass: i64) -> i64 {
    // You can return early from a function by using the `return` keyword and
    // specifying a value, but most functions return the last expression
    // implicitly. Note that expressions don't have semicolons.
    //
    // https://doc.rust-lang.org/book/ch03-03-how-functions-work.html

    // To find the fuel required for a module, take its mass, divide by three,
    // round down, and subtract 2.
    // 
    // I like how Rust warned me against using an unsigned int:
    // thread 'main' panicked at 'attempt to subtract with overflow'
    let fuel_mass = (mass / 3) - 2;

    // Can't do a one-line if-statement in Rust. Good to know.
    if fuel_mass <= 0 { return 0; }

    // The fuel also requires fuel...
    return fuel_mass + fuel_needed_for_mass(fuel_mass);
}

fn main() {
    assert_eq!(fuel_needed_for_mass(14), 2);
    assert_eq!(fuel_needed_for_mass(1969), 966);
    assert_eq!(fuel_needed_for_mass(100756), 50346);

    let mut total_fuel_needed: i64 = 0;
    // Functions return `Result` whenever errors are expected and recoverable.
    // `Ok(T)` represents success and contains a value, while `Err(E)`
    // represents an error and contains an error value.
    // https://doc.rust-lang.org/std/result/
    if let Ok(lines) = read_lines("./input01.txt") {
        for line in lines {
            // So much error handling. In Python, I couldn't even consider
            // adding this check. What could go wrong at this point?
            // Update: say the line is not valid UTF-8
            if let Ok(mass_str) = line {
                let mass = mass_str.parse::<i64>().unwrap();
                total_fuel_needed += fuel_needed_for_mass(mass);
            }
        }
    }
    // Macros look like functions, except that their name ends with
    // a bang !, but instead of generating a function call, macros
    // are expanded into source code that gets compiled with the
    // rest of the program.
    // https://doc.rust-lang.org/rust-by-example/macros.html
    // 
    // The string formatting is similar to that of Python
    // https://doc.rust-lang.org/std/fmt/index.html
    println!("Total fuel needed: {}", total_fuel_needed);
}

/// Returns an `Iterator` to the `Reader` of the lines of the file. The output
/// is wrapped in an `io::Result<T>` to make the failure of all I/O operations
/// explicit.
/// 
/// Source: https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html#read_lines
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    // From the docs: `BufReader<R>` can improve the speed of programs that make
    // small and repeated read calls to the same file. It does not help when
    // reading very large amounts at once, or reading just one or a few times.
    // It also provides no advantage when reading from a source that is already
    // in memory, like a `Vec<u8>`.
    Ok(io::BufReader::new(file).lines())
}