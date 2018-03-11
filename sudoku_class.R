library(R6)

source("sudoku_functions.R")

grid <- 
  R6Class("Grid",
          public = list(
            vec = NULL,
            complete = NULL,
            children = NULL,
            
            initialize = function(vec = NA){
              self$vec <- vec
              self$iscomplete()
            },
            
            iscomplete = function(){
              self$complete <- is_complete(self$vec)
            },
            
            print = function(){
              print(self$vec)
              print(plot_matrix(self$vec))
            }
          ))

G1 <- grid$new(V_complete)
G1

# Rajouter is_full/isfull
