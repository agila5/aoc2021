# --- Day 4: Giant Squid --- ----------------------------------------------

# Part 1 --------------------------------------------------------
example <- readLines("data/input04.txt")
numbers <- strsplit(example[1], ",")[[1]] # the numbers that will be extracted
example <- example[-1]
length_board <- 6 # 5 rows + 1 blank line at the top
n_boards <- length(example) / length_board # the first element contains the numbers
# save all boards into an array. I need trimws to remove blank spaced at the
# beginnin of the string + I need to remove more than one spaces in cases line 8
# 7 with two spaces
boards <- array(unlist(strsplit(trimws(example), "[[:space:]]+")), dim = c(5, 5, n_boards))

while (TRUE) {
  # Extract one number
  drawn <- numbers[1]
  
  # Mark numbers that are extracted
  idx_drawn <- which(boards == drawn)
  boards[idx_drawn] <- "DRAWN"
  
  if (any(colSums(aperm(boards == "DRAWN", c(2, 1, 3))) == 5L)) {
    idx_which_row <- which(
      colSums(aperm(boards == "DRAWN", c(2, 1, 3))) == 5L,
      arr.ind = TRUE
    )
    break
  }
  
  if (any(colSums(boards == "DRAWN") == 5L)) {
    idx_which_col <- which(
      colSums(boards == "DRAWN") == 5L, 
      arr.ind = TRUE
    )
    break
  }
  
  # Remove number
  numbers <- numbers[-1]
}

# Check
idx_which_row
boards[3, , 88]

# Compute sum
sum(as.numeric(boards[, , 88]), na.rm = TRUE) * as.numeric(drawn)

# Part 2 --------------------------------------------------------
rm(list = ls())
example <- readLines("data/input04.txt")
numbers <- strsplit(example[1], ",")[[1]]
example <- example[-1]
length_board <- 6
n_boards <- length(example) / length_board
boards <- array(unlist(strsplit(trimws(example), "[[:space:]]+")), dim = c(5, 5, n_boards))

while(length(dim(boards)) > 2) {
  # Extract one number
  drawn <- numbers[1]
  
  # Mark numbers that are extracted
  idx_drawn <- which(boards == drawn)
  boards[idx_drawn] <- "DRAWN"
  
  if (any(colSums(aperm(boards == "DRAWN", c(2, 1, 3))) == 5L)) {
    idx_which_row <- which(
      colSums(aperm(boards == "DRAWN", c(2, 1, 3))) == 5L,
      arr.ind = TRUE
    )
    boards <- boards[, , -idx_which_row[1, 2]]
    next
  }
  
  if (any(colSums(boards == "DRAWN") == 5L)) {
    idx_which_col <- which(
      colSums(boards == "DRAWN") == 5L, 
      arr.ind = TRUE
    )
    boards <- boards[, , -idx_which_col[1, 2]]
    next
  }
  
  # Remove number
  numbers <- numbers[-1]
}

# Check 
boards

while (TRUE) {
  drawn <- numbers[1]
  idx_drawn <- which(boards == drawn)
  boards[idx_drawn] <- "DRAWN"
  if (any(rowSums(boards == "DRAWN") == 5L)) {
    idx_which_row <- which(
      colSums(boards == "DRAWN") == 5L,
      arr.ind = TRUE
    )
    break
  }
  if (any(colSums(boards == "DRAWN") == 5L)) {
    idx_which_col <- which(
      colSums(boards == "DRAWN") == 5L, 
      arr.ind = TRUE
    )
    break
  }
  numbers <- numbers[-1]
}

# Check 
boards

# Compute
sum(as.numeric(boards), na.rm = TRUE) * as.numeric(drawn)
