# --- Day 1: Sonar Sweep --- ----------------------------------------------

# Example - Part 1
input <- c(199L, 200L, 208L, 210L, 200L, 207L, 240L, 269L, 260L, 263L)
sum(diff(input) > 0)

# Input - Part 1 ----------------------------------------------------------
input <- as.numeric(readLines("data/input01.txt"))
sum(diff(input) > 0)

# Example - Part 2 --------------------------------------------------------
input <- c(199L, 200L, 208L, 210L, 200L, 207L, 240L, 269L, 260L, 263L)
sum(diff(zoo::rollapply(input, 3, sum)) > 0)

# Input - Part 2 ----------------------------------------------------------
input <- as.numeric(readLines("data/input01.txt"))
sum(diff(zoo::rollapply(input, 3, sum)) > 0)
