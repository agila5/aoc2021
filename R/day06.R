# --- Day 6: Lanternfish --- ----------------------------------------------

# Part 1 ------------------------------------------------------------------

# Each lanternfish creates a new lanternfish once every 7 days. However, this
# process isn't necessarily synchronized between every lanternfish - one
# lanternfish might have 2 days left until it creates another lanternfish, while
# another might have 4. So, you can model each fish as a single number that
# represents the number of days until it creates a new lanternfish. 

# Furthermore, you reason, a new lanternfish would surely need slightly longer
# before it's capable of producing more lanternfish: two more days for its first
# cycle.

# So, suppose you have a lanternfish with an internal timer value of 3:
  
# After one day, its internal timer would become 2.
# After another day, its internal timer would become 1.
# After another day, its internal timer would become 0.
# After another day, its internal timer would reset to 6, and it would create a
# new lanternfish with an internal timer of 8.
# After another day, the first lanternfish would have an internal timer of 5,
# and the second lanternfish would have an internal timer of 7.

# A lanternfish that creates a new fish resets its timer to 6, not 7 (because 0
# is included as a valid timer value). The new lanternfish starts with an
# internal timer of 8 and does not start counting down until the next day.

input <- readLines("data/input06.txt")
input <- as.integer(unlist(strsplit(input, ",")))
input_table <- table(factor(input, levels = 0:8))

# This list means that the first fish has an internal timer of 3, the second
# fish has an internal timer of 4, and so on until the fifth fish, which has an
# internal timer of 2.

time <- 1L

while(time <= 80L) {
  if (input_table[[1]] > 0L) { # input_table[[1]] is the number of zeros
    zero_counts <- input_table[[1]]
    input_table[1:8] <- input_table[2:9]
    input_table[[7]] <- input_table[[7]] + zero_counts
    input_table[[9]] <- zero_counts
  } else {
    input_table[1:8] <- input_table[2:9]
    input_table[[9]] <- 0L
  }
  
  print(paste0("Time is: ", time))
  time <- time + 1L
}

sum(input_table)

# Part 2 ------------------------------------------------------------------
input <- readLines("data/input06.txt")
input <- as.numeric(unlist(strsplit(input, ",")))
input_table <- table(factor(input, levels = 0:8))
time <- 1L

while(time <= 256L) {
  if (input_table[[1]] > 0) { # input_table[[1]] is the number of zeros
    zero_counts <- input_table[[1]]
    input_table[1:8] <- input_table[2:9]
    input_table[[7]] <- input_table[[7]] + zero_counts
    input_table[[9]] <- zero_counts
  } else {
    input_table[1:8] <- input_table[2:9]
    input_table[[9]] <- 0
  }
  
  print(paste0("Time is: ", time))
  time <- time + 1L
}

sprintf("%13.f", sum(input_table))
