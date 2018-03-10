library(ggplot2)

### Plot matrix from the vector form (vec) ----
## Display the grid in a matric form

plot_matrix <- function(vec) {
  df <- data.frame(Vec = vec, X = 0:80 %/% 9, Y = 9 - 0:80 %% 9)
  
  ggplot(df, aes(X, Y)) +
    geom_text(aes(label = Vec)) +
    geom_vline(xintercept = c(2.5, 5.5)) +
    geom_hline(yintercept = c(3.5, 6.5)) +
    coord_fixed() +
    theme_void()
}

# Exemple

set.seed(42)

(V_full <- sample(c(1:9), 81, replace = TRUE))
(V_na <- sample(c(NA, 1:9), 81, replace = TRUE))
(V_num <- 1:81)
V_quot <- 0:80 %/% 9
V_rest <- 0:80 %% 9


plot_matrix(V_full)
plot_matrix(V_na)
plot_matrix(V_num)
plot_matrix(V_quot)
plot_matrix(V_rest)


### Create indexes for columns, rows and squares ----
## Lists of vector elements belonging to each rows, columns and squares

L_cols <- list()
L_rows <- list()
L_squares <- list()

for(i in 1:9){
  L_cols[[i]] <- which(0:80 %/% 9 == i-1)
  L_rows[[i]] <- which(0:80 %% 9 == i-1)
}

for(i in 1:3){
  for(j in 1:3){
    L_squares[[3*(j-1)+i]] <- 
      which((0:80 %% 9) %in% ((3*(i-1)):(3*i-1)) & 
              (0:80 %/% 9) %in% ((3*(j-1)):(3*j-1)))
  }
}

L_all <- c(L_cols, L_rows, L_squares)

### Get grid position (c, r, s) from vector position (n) ----
## Returns a list made up of row, column and square numbers

grid_position <- function(n){
  c <- (n-1) %/% 9 + 1
  r <- (n-1) %% 9 + 1
  s <- 3 * ((c-1) %/% 3) + (r-1) %/% 3 + 1
  list(c, r, s)
}

# grid_position_c <- function(n){
#   c <- (n-1) %/% 9 + 1
#   r <- (n-1) %% 9 + 1
#   s <- 3 * ((c-1) %/% 3) + (r-1) %/% 3 + 1
#   c(c, r,  s)
# }
# 
# microbenchmark(grid_position(42), grid_position_c(42), times = 10000)
# It's faster with list !

# Exemple

grid_position(1)
grid_position(2)
grid_position(4)
grid_position(10)
grid_position(42)
grid_position(81)


### Get first empty position (n) ----
## Returns NA if the grid is full

first_empty <- function(vec){
  which(is.na(vec))[1]
}

# Exemple

first_empty(V_na)
first_empty(V_full)

grid_position(first_empty(V_na))


### Give the authorized numbers for a position ----
## Returns integer(0) if the completion is not possible

authorized_numbers <- function(vec, n){
  crs_position <- grid_position(n)
  indexes <- 
    c(L_cols[[crs_position[[1]]]], L_rows[[crs_position[[2]]]], L_squares[[crs_position[[3]]]])
  unauthorized_numbers <- vec[indexes]
  setdiff(1:9, unauthorized_numbers)
}

# Example

plot_matrix(V_full)
authorized_numbers(V_full, 4)

plot_matrix(V_na)
authorized_numbers(V_na, 30)


### Check if a vector (vec) is complete ----
## Return TRUS/FALSE

is_complete <- function(vec){
  bool <- sapply(L_all, function(x) setequal(1:9, vec[x]))
  all(bool)
}

# Example

is_complete(V_num)


