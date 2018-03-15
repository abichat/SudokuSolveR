library(tidyverse)

#### Notations ----

# vec and V_* are grid in vector form
# p is the position of an element in the vector (between 1 and 81)
# c, r and s are the number of the column, 
  # row and square of an element in the grid (between 0 and 9)
# 

### Plot matrix from the vector form (vec) ----
## Display the grid in a matrix form

plot_matrix <- function(vec) {
  
  # Need to create a function with this snip
  
  if (class(vec) == "list") {
    vec <- vec$vector
  }
  
  df <- data.frame(vec = vec, X = 0:80 %/% 9, Y = 9 - 0:80 %% 9)
  
  ggplot(df, aes(X, Y)) +
    geom_text(aes(label = vec), na.rm = TRUE) +
    geom_vline(xintercept = c(2.5, 5.5)) +
    geom_hline(yintercept = c(3.5, 6.5)) +
    coord_fixed() +
    theme_void() 
}

# Example

set.seed(42)

V_full <- sample(c(1:9), 81, replace = TRUE)
V_na <- sample(c(NA, 1:9), 81, replace = TRUE)
V_num <- 1:81
V_quot <- 0:80 %/% 9
V_rest <- 0:80 %% 9
V_complete <- c(1:9, 7:9, 1:6, 4:9, 1:3, 
                9, 1:8, 6:9, 1:5, 3:9, 1:2,
                8:9, 1:7, 5:9, 1:4, 2:9, 1)
V_almostcomp <- V_complete; V_almostcomp[c(1,2, 73)] <- NA
V_almostcomp2 <- V_complete; V_almostcomp2[c(12, 21, 18)] <- NA
V_realgrid <- c(NA, NA, 9, 3, 6, 4, 8, NA, 2,
                NA, 6, NA, NA, 7, NA, 4, 3, 9,
                3, rep(NA, 5), 7, 1, rep(NA, 4),
                4, NA, 7, NA, NA, 1,
                NA, 9, NA, 2:3, 6, NA, 8, NA,
                6, NA, NA, 1, NA, 5, rep(NA, 4),
                8, 6, rep(NA, 5), 5,
                9, 5, 3, NA, 4, NA, NA, 7, NA,
                1, NA, 4, 8, 5, 9, 2, NA, NA)
V_realgridcomp <- c(7, 1, 9, 3, 6, 4, 8, 5, 2,
                    8, 6, 2, 5, 7, 1, 4, 3, 9,
                    3, 4, 5, 9, 2, 8, 7, 1, 6,
                    5, 3, 8, 4, 9, 7, 6, 2, 1,
                    4, 9, 1, 2, 3, 6, 5, 8, 7,
                    6, 2, 7, 1, 8, 5, 3, 9, 4,
                    2, 8, 6, 7, 1, 3, 9, 4, 5,
                    9, 5, 3, 6, 4, 2, 1, 7, 8,
                    1, 7, 4, 8, 5, 9, 2, 6, 3)
V_impossible <- c(NA, NA, 3:9, rep(NA, 18), 2, rep(NA, 53))
# http://la-conjugaison.nouvelobs.com/sudoku/grille.php?niveau=difficile&grille=2522
V_hardcore <- c(NA, NA, 1, NA, 6, rep(NA, 4),
                5, NA, 2, NA, 9, 4, rep(NA, 4),
                8:9, 3, rep(NA, 3), 6, NA,
                9, NA, 3, 5, NA, 1, rep(NA, 4),
                5, rep(NA, 5), 1, rep(NA, 4),
                4, NA, 6, 3, NA, 5,
                NA, 7, rep(NA, 3), 8:9, 3, rep(NA, 4),
                6, 1, NA, 2, NA, 8,
                rep(NA, 4), 5, NA, 1, NA, NA)
V_hardcoremultiple <- c(NA, NA, NA, NA, NA, rep(NA, 4),
                5, NA, 2, NA, 9, 4, rep(NA, 4),
                8:9, 3, rep(NA, 3), 6, NA,
                9, NA, 3, 5, NA, 1, rep(NA, 4),
                5, rep(NA, 5), 1, rep(NA, 4),
                4, NA, 6, 3, NA, 5,
                NA, 7, rep(NA, 3), 8:9, 3, rep(NA, 4),
                6, 1, NA, 2, NA, 8,
                rep(NA, 4), NA, NA, 1, NA, NA)



plot_matrix(V_full)
plot_matrix(V_na)
plot_matrix(V_num)
plot_matrix(V_quot)
plot_matrix(V_rest)
plot_matrix(V_complete)
plot_matrix(V_almostcomp)
plot_matrix(V_almostcomp2)
plot_matrix(V_realgrid)
plot_matrix(V_realgridcomp)
plot_matrix(V_impossible)
plot_matrix(V_hardcore)




### Create indexes for columns, rows and squares ----
## Lists of vector elements belonging to each rows, columns and squares

L_cols <- vector("list", 9)
L_rows <- vector("list", 9)
L_squares <- vector("list", 9)

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

rm(i, j)

L_all <- c(L_cols, L_rows, L_squares)
### Get grid position (c, r, s) from vector position (p) ----
## Returns a list made up of row, column and square numbers

grid_position <- function(p){
  c <- (p-1) %/% 9 + 1
  r <- (p-1) %% 9 + 1
  s <- 3 * ((c-1) %/% 3) + (r-1) %/% 3 + 1
  list(c, r, s)
}

# grid_position_c <- function(p){
#   c <- (p-1) %/% 9 + 1
#   r <- (p-1) %% 9 + 1
#   s <- 3 * ((c-1) %/% 3) + (r-1) %/% 3 + 1
#   c(c, r,  s)
# }
# 
# m <- microbenchmark(grid_position(42), grid_position_c(42), times = 10000)
# wilcox.test(pull(filter(m, expr == "grid_position_list(42)"), time), pull(filter(m, expr == "grid_position_c(42)"), time), paired = FALSE)
# ggplot(m, aes(expr, time)) + geom_lv() + scale_y_log10() + theme_bw() + labs(caption = "N = 10000\np < 2.2e-16 (t-test & Wilcoxon test)")
# It's faster with list !

# Example

grid_position(1)
grid_position(2)
grid_position(4)
grid_position(10)
grid_position(42)
grid_position(81)



### Get all empty position (p) ----
## Returns integer(0) if the grid is full

empty_cases <- function(vec){
  which(is.na(vec))
}

# Example

empty_cases(V_na)
empty_cases(V_full)
length(empty_cases(V_full))


### Get first empty position (p) ----
## Returns NA if the grid is full
# A supprimer ? 

first_empty <- function(vec){
  which(is.na(vec))[1]
}

# Example

first_empty(V_na)
first_empty(V_full)

grid_position(first_empty(V_na))


### Give the authorized numbers for a position (p) ----
## Returns integer(0) if the completion is not possible

authorized_numbers <- function(vec, p){
  crs_position <- grid_position(p)
  indexes <- 
    c(L_cols[[crs_position[[1]]]], L_rows[[crs_position[[2]]]], L_squares[[crs_position[[3]]]])
  unauthorized_numbers <- vec[indexes]
  base::setdiff(1:9, unauthorized_numbers)
}

# Example

plot_matrix(V_full)
authorized_numbers(V_full, 4)

plot_matrix(V_na)
authorized_numbers(V_na, 30)

plot_matrix(V_almostcomp)
authorized_numbers(V_almostcomp, 1)

plot_matrix(V_almostcomp2)
authorized_numbers(V_almostcomp2, 12)
authorized_numbers(V_almostcomp2, 18)


### Check if a vector (vec) is complete (ie full without repetition) ----
## Return TRUS/FALSE

is_complete <- function(vec){
  bool <- sapply(L_all, function(x) setequal(1:9, vec[x]))
  all(bool)
}

# Example

is_complete(V_almostcomp)
is_complete(V_complete)
is_complete(V_realgridcomp)


### Add numbers (n) at positions (p) in a vector (vec) ----
## Return the completed matrix

add_n <- function(vec, n, p){
  vec[p] <- n
  vec
}

plot_matrix(V_almostcomp2)
plot_matrix(add_n(V_almostcomp2, c(9, 6, 6), c(12, 18, 21)))
is_complete(add_n(V_almostcomp2, c(9, 6, 6), c(12, 18, 21)))


### Fill all unambiguous cases ----
## Could be run several times in succession
# Need to deal woth $vector and $success

fill_unambiguous_cases <- function(vec){
  
  if (class(vec) == "list") {
    vec <- vec$vector
  }
  
  df <- tibble(p = empty_cases(vec)) %>% 
    mutate(n = map(p, ~ authorized_numbers(vec, .)),
           l = map_int(n, length)) %>% 
    filter(l == 1)
  
  ifelse(nrow(df) > 0, succ <- TRUE, succ <- FALSE)
  
  if (nrow(df) > 0) {
    df <- mutate(df, n = unlist(n))
    return(list(vector = add_n(vec, df$n, df$p),
                success = TRUE))
  } else {
    return(list(vector = vec,
                success = FALSE))
  }
}

plot_matrix(V_almostcomp)
fill_unambiguous_cases(V_almostcomp)
plot_matrix(fill_unambiguous_cases(V_almostcomp))


plot_matrix(V_realgrid)
V_realgridcontd <- fill_unambiguous_cases(V_realgrid)
plot_matrix(V_realgridcontd)
V_realgridcontd <- fill_unambiguous_cases(V_realgridcontd)
plot_matrix(V_realgridcontd)
V_realgridcontd <- fill_unambiguous_cases(V_realgridcontd)
plot_matrix(V_realgridcontd)
V_realgridcontd <- fill_unambiguous_cases(V_realgridcontd)
plot_matrix(V_realgridcontd)
V_realgridcontd <- fill_unambiguous_cases(V_realgridcontd)
plot_matrix(V_realgridcontd)
V_realgridcontd <- fill_unambiguous_cases(V_realgridcontd)
plot_matrix(V_realgridcontd)
V_realgridcontd <- fill_unambiguous_cases(V_realgridcontd)
plot_matrix(V_realgridcontd)
fill_unambiguous_cases(V_realgridcomp)


### ----

fill_unambiguous_cases2 <- function(vec){
  
  if (class(vec) == "list") {
    vec <- vec$vector
  }
  
  vec <- fill_unambiguous_cases(vec)
  while (vec$success) {
    vec <- fill_unambiguous_cases(vec$vector)
  }
  return(vec)
}


plot_matrix(V_realgrid)
fill_unambiguous_cases2(V_realgrid)
plot_matrix(fill_unambiguous_cases2(V_realgrid))
plot_matrix(V_realgridcomp)

# PROBLEM !!!!
plot_matrix(fill_unambiguous_cases2(V_impossible)$vector)

