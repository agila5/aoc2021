# --- Day 9: Smoke Basin --- ----------------------------------------------
library(igraph)
validate_points <- function(..., .dim) {
  points <- do.call(rbind, list(...))
  valid_points <- points[, 1] >= 1 & points[, 1] <= .dim[1L] & points[, 2] >= 1 & points[, 2] <= .dim[2L]
  points[valid_points, ]
}

# Part 1 ------------------------------------------------------------------
input <- readLines("data/input09.txt")
input <- do.call(rbind, strsplit(input, ""))

# Compute the neighbours list
neigh_list <- apply(
  X = arrayInd(seq_along(input), dim(input)), 
  MARGIN = 1, 
  FUN = function(xy) {
    x <- xy[1]; y <- xy[2]
    left <- c(x - 1, y); right <- c(x + 1, y)
    top <- c(x, y + 1); bottom <- c(x, y - 1)
    points <- validate_points(left, top, bottom, right, .dim = dim(input))
    vector_id <- points[, 1, drop = TRUE] + (points[, 2, drop = TRUE] - 1L) * dim(input)[2L] 
  }, 
  simplify = FALSE
)

# Create the graph
my_graph <- graph_from_adj_list(neigh_list)

# Add heights
V(my_graph)$height <- input

# Create the matrix for local minimum and compute local minima
local_min <- matrix(FALSE, nrow = nrow(input), ncol = ncol(input))

for (i in seq_along(local_min)) {
  ith_height <- V(my_graph)$height[i]
  id_neigh <- V(my_graph)[.nei(i)]
  neigh_height <- V(my_graph)$height[id_neigh]
  
  if (all(ith_height < neigh_height)) {
    local_min[i] <- TRUE
  }
}

sum(as.numeric(input[local_min]) + 1)

# Part 2 ------------------------------------------------------------------

# Remove 9s
my_graph <- delete_vertices(my_graph, V(my_graph)$height == "9")

# Compute components and their size
components(my_graph)

# Sort and take products
prod(sort(components(my_graph)$csize, decreasing = TRUE)[1:3])

