# --- Day 7: The Treachery of Whales --- ----------------------------------

# Part 1 ------------------------------------------------------------------

input <- as.integer(strsplit(readLines("data/input07.txt"), ",")[[1]])
costs <- rowSums(outer(0:1000, input, function(x, y) abs(x - y)))
which.min(costs) - 1L; min(costs)

# Part 2 ------------------------------------------------------------------

input <- as.integer(strsplit(readLines("data/input07.txt"), ",")[[1]])
costs <- rowSums(outer(
  X = 0:500, 
  Y = input, 
  FUN = function(x, y) {
    dist <- abs(x - y)
    cost <- dist * (dist + 1) / 2
    cost
  }
))
which.min(costs) - 1L; min(costs)

