# --- Day 8: Seven Segment Search --- -------------------------------------

# Part 1 ------------------------------------------------------------------
input <- readLines("data/input08.txt")
input <- sapply(strsplit(input, " | ", fixed = TRUE), `[`, 2)
input <- unlist(sapply(strsplit(input, " "), nchar, simplify = FALSE))
sum((input == 2) + (input == 4) + (input == 3) + (input == 7))

# Part 2 ------------------------------------------------------------------
input <- "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"


