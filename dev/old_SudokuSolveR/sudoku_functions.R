library(tidyverse)

#### Notations ----

# vec and V_* are grid in vector form
# p is the position of an element in the vector (between 1 and 81)
# c, r and s are the number of the column, 
  # row and square of an element in the grid (between 0 and 9)
# 

### Example matrix ----

V_num <- 1:81

V_realgrid <- c(NA, NA, 9, 3, 6, 4, 8, NA, 2,
                NA, 6, NA, NA, 7, NA, 4, 3, 9,
                3, rep(NA, 5), 7, 1, rep(NA, 4),
                4, NA, 7, NA, NA, 1,
                NA, 9, NA, 2:3, 6, NA, 8, NA,
                6, NA, NA, 1, NA, 5, rep(NA, 4),
                8, 6, rep(NA, 5), 5,
                9, 5, 3, NA, 4, NA, NA, 7, NA,
                1, NA, 4, 8, 5, 9, 2, NA, NA)

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


### Plot matrix from the vector form (vec) ----
## Display the grid in a matrix form

plot_matrix <- function(vec, vec_na = rep(NA, length(vec)), fillcolor = "#03a9f4") {
  
  # Need to create a function with this snip
  
  if (class(vec) == "list") {
    vec <- vec$vector
  }
  
  df <- data.frame(vec = vec, X = 0:80 %/% 9, Y = 9 - 0:80 %% 9,
                   na = is.na(vec_na))
  
  ggplot(df, aes(X, Y)) +
    geom_rect(aes(xmin = X - 0.5, xmax = X + 0.5, ymin = Y - 0.5, ymax = Y + 0.5,
                  fill = na), alpha = 0.5) +
    scale_fill_manual(values = c("white", fillcolor)) +
    guides(fill = FALSE) +
    geom_text(aes(label = vec), na.rm = TRUE) +
    geom_vline(xintercept = c(2.5, 5.5)) +
    geom_hline(yintercept = c(3.5, 6.5)) +
    coord_fixed() +
    theme_void() 
}


plot_matrix(V_realgrid, V_realgrid)

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


### Give the authorized numbers for a position (p) ----
## Returns integer(0) if the completion is not possible

authorized_numbers <- function(vec, p){
  crs_position <- grid_position(p)
  indexes <- 
    c(L_cols[[crs_position[[1]]]], L_rows[[crs_position[[2]]]], L_squares[[crs_position[[3]]]])
  unauthorized_numbers <- vec[indexes]
  base::setdiff(1:9, unauthorized_numbers)
}


### Check if a vector (vec) is complete (ie full without repetition) ----
## Return TRUS/FALSE

is_complete <- function(vec){
  bool <- sapply(L_all, function(x) setequal(1:9, vec[x]))
  all(bool)
}


