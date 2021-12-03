# --- Day 3: Binary Diagnostic --- ----------------------------------------

# Example - Part 1 --------------------------------------------------------
input <- c("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
input <- do.call(rbind, strsplit(input, ""))

gamma_rate <- apply(input, 2, FUN = function(x) names(sort(table(x), decreasing = TRUE))[[1]])
gamma_rate <- strtoi(paste(gamma_rate, collapse = ""), base = 2L)

epsilon_rate <- apply(input, 2, FUN = function(x) names(sort(table(x), decreasing = FALSE))[[1]])
epsilon_rate <- strtoi(paste(epsilon_rate, collapse = ""), base = 2L)

gamma_rate * epsilon_rate

# Input - Part 1 ----------------------------------------------------------
input <- readLines("data/input03.txt")
input <- do.call(rbind, strsplit(input, ""))

gamma_rate <- apply(input, 2, FUN = function(x) names(sort(table(x), decreasing = TRUE))[[1]])
gamma_rate <- strtoi(paste(gamma_rate, collapse = ""), base = 2L)

epsilon_rate <- apply(input, 2, FUN = function(x) names(sort(table(x), decreasing = FALSE))[[1]])
epsilon_rate <- strtoi(paste(epsilon_rate, collapse = ""), base = 2L)

gamma_rate * epsilon_rate

# Example - Part 2 --------------------------------------------------------
rm(list = ls())
input <- c("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
input <- do.call(rbind, strsplit(input, ""))

oxygen <- input
i <- 1L
while(nrow(oxygen) > 1) {
  # Determine the most common bit for the ith column
  bit_table <- table(oxygen[, i])
  if (length(bit_table) > 1 && bit_table[1] == bit_table[2]) {
    most_common_bit = "1"
  } else {
    most_common_bit <- names(sort(bit_table, decreasing = TRUE))[[1]]
  }
  
  # Remove the rows where the value of the ith column is not equal to the most common bit
  oxygen <- oxygen[oxygen[, i] == most_common_bit, , drop = FALSE]
  
  # Check next column
  i <- i + 1
}

# Same stuff for CO2
CO2 <- input
i <- 1L
while(nrow(CO2) > 1) {
  # Determine the least common bit for the ith column
  bit_table <- table(CO2[, i])
  if (length(bit_table) > 1 && bit_table[1] == bit_table[2]) {
    least_common_bit = "0"
  } else {
    least_common_bit <- names(sort(bit_table, decreasing = FALSE))[[1]]
  }
  
  # Remove the rows where the value of the ith column is not equal to the most common bit
  CO2 <- CO2[CO2[, i] == least_common_bit, , drop = FALSE]
  
  # Check next column
  i <- i + 1
}

oxygen <- strtoi(paste(oxygen, collapse = ""), base = 2L)
CO2 <- strtoi(paste(CO2, collapse = ""), base = 2L)

oxygen * CO2

# Input - Part 1 ----------------------------------------------------------
input <- readLines("data/input03.txt")
input <- do.call(rbind, strsplit(input, ""))

oxygen <- input
i <- 1L
while(nrow(oxygen) > 1) {
  # Determine the most common bit for the ith column
  bit_table <- table(oxygen[, i])
  if (length(bit_table) > 1 && bit_table[1] == bit_table[2]) {
    most_common_bit = "1"
  } else {
    most_common_bit <- names(sort(bit_table, decreasing = TRUE))[[1]]
  }
  
  # Remove the rows where the value of the ith column is not equal to the most common bit
  oxygen <- oxygen[oxygen[, i] == most_common_bit, , drop = FALSE]
  
  # Check next column
  i <- i + 1
}

# Same stuff for CO2
CO2 <- input
i <- 1L
while(nrow(CO2) > 1) {
  # Determine the least common bit for the ith column
  bit_table <- table(CO2[, i])
  if (length(bit_table) > 1 && bit_table[1] == bit_table[2]) {
    least_common_bit = "0"
  } else {
    least_common_bit <- names(sort(bit_table, decreasing = FALSE))[[1]]
  }
  
  # Remove the rows where the value of the ith column is not equal to the most common bit
  CO2 <- CO2[CO2[, i] == least_common_bit, , drop = FALSE]
  
  # Check next column
  i <- i + 1
}

oxygen <- strtoi(paste(oxygen, collapse = ""), base = 2L)
CO2 <- strtoi(paste(CO2, collapse = ""), base = 2L)

oxygen * CO2
