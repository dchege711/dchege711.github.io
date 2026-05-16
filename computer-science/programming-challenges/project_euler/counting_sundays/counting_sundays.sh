gn gen out/default

# Generate a compilation database
# https://sarcasm.github.io/notes/dev/compilation-database.html#ninja
ninja -C out/default -t compdb > compile_commands.json

# Build and run the executable
ninja -C out/default counting_sundays
out/default/counting_sundays
