# --- Day 5: Hydrothermal Venture --- -------------------------------------

# Auxiliary functions
list_h_points <- function(x) {
  paste0(seq(x[1], x[3]), "-", x[2])
}
list_v_points <- function(x) {
  paste0(x[1], "-", seq(x[2], x[4]))
}
list_d_points <- function(x) {
  paste0(
    seq(x[1], x[3]), 
    "-", 
    seq(x[2], x[4])
  )
}

# Part 1 ------------------------------------------------------------------
input <- readLines("data/input05.txt")
input <- do.call(rbind, strsplit(gsub("(,|\\s->\\s)", " ", input), " "))

# find horizontal and vertical lines
is_v <- input[, 1] == input[, 3]
is_h <- input[, 2] == input[, 4]
input <- input[is_v | is_h, ]

# Loop over all line segments, list all points and add those points to a list
# counting the occurrences
list_points <- list()
pb <- txtProgressBar(min = 1, max = nrow(input), initial = 1, style = 3)
for (i in seq_len(nrow(input))) {
  points <- if (input[i, 1] == input[i, 3]) {
    list_v_points(input[i, ])
  } else if (input[i, 2] == input[i, 4]) {
    list_h_points(input[i, ])
  }
  
  for (point in points) {
    if (point %in% names(list_points)) {
      list_points[[point]] = list_points[[point]] + 1L
    } else {
      list_points[point] = 1L
    }
  }
  setTxtProgressBar(pb, i)
}

sum(list_points > 1)

# Part 2 ------------------------------------------------------------------
input <- readLines("data/input05.txt")
input <- do.call(rbind, strsplit(gsub("(,|\\s->\\s)", " ", input), " "))

# Loop over all line segments, list all points and add those points to a list
# counting the occurrences
list_points <- list()
pb <- txtProgressBar(min = 1, max = nrow(input), initial = 1, style = 3)
for (i in seq_len(nrow(input))) {
  points <- if (
    input[i, 1] == input[i, 3]
  ) {
    list_v_points(input[i, ])
  } else if (
    input[i, 2] == input[i, 4]
  ) {
    list_h_points(input[i, ])
  } else {
    list_d_points(input[i, ])
  }
  
  for (point in points) {
    if (point %in% names(list_points)) {
      list_points[[point]] = list_points[[point]] + 1L
    } else {
      list_points[point] = 1L
    }
  }
  setTxtProgressBar(pb, i)
}

sum(list_points > 1)

