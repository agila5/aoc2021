# --- Day 2: Dive! --- ----------------------------------------------------

# Example - Part 1 --------------------------------------------------------
input <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

# Convert into a matrix. First column in the direction (forward, down and up),
# second column is the value
input <- do.call(rbind, strsplit(input, " "))

# Creates movements matrix
movements <- matrix(0, nrow = nrow(input), ncol = 2)

# and compute all movements
idx_forward <- input[, 1] == "forward"
movements[idx_forward, 1] <- as.numeric(input[idx_forward, 2])
idx_down <- input[, 1] == "down"
movements[idx_down, 2] <- as.numeric(input[idx_down, 2])
idx_up <- input[, 1] == "up"
movements[idx_up, 2] <- (-1) * as.numeric(input[idx_up, 2])

# result
prod(colSums(movements))

# Input - Part 1 ----------------------------------------------------------
input <- readLines("data/input02.txt")
input <- do.call(rbind, strsplit(input, " "))
movements <- matrix(0, nrow = nrow(input), ncol = 2)
idx_forward <- input[, 1] == "forward"
movements[idx_forward, 1] <- as.numeric(input[idx_forward, 2])
idx_down <- input[, 1] == "down"
movements[idx_down, 2] <- as.numeric(input[idx_down, 2])
idx_up <- input[, 1] == "up"
movements[idx_up, 2] <- (-1) * as.numeric(input[idx_up, 2])
prod(colSums(movements))

# Example - Part 2 --------------------------------------------------------
rm(list = ls())
input <- c("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
input <- do.call(rbind, strsplit(input, " "))

# Creates position/aim vector
pos <- c(0, 0)
aim <- 0

# loop over all steps
for (i in seq_len(nrow(input))) {
  if (input[i, 1] == "forward") {
    pos[1] <- pos[1] + as.numeric(input[i, 2])
    pos[2] <- pos[2] + aim * as.numeric(input[i, 2])
  } else if (input[i, 1] == "down") {
    aim <- aim + as.numeric(input[i, 2])
  } else if (input[i, 1] == "up") {
    aim <- aim - as.numeric(input[i, 2])
  } else {
    stop("AAAAARGH")
    }
}

pos[1] * pos[2]

# Input - Part 2 ----------------------------------------------------------
input <- readLines("data/input02.txt")
input <- do.call(rbind, strsplit(input, " "))

# Creates position/aim vector
pos <- c(0, 0)
aim <- 0

# loop over all steps
for (i in seq_len(nrow(input))) {
  if (input[i, 1] == "forward") {
    pos[1] <- pos[1] + as.numeric(input[i, 2])
    pos[2] <- pos[2] + aim * as.numeric(input[i, 2])
  } else if (input[i, 1] == "down") {
    aim <- aim + as.numeric(input[i, 2])
  } else if (input[i, 1] == "up") {
    aim <- aim - as.numeric(input[i, 2])
  } else {
    stop("AAAAARGH")
  }
}

pos[1] * pos[2]
