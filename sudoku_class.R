library(R6)

source("sudoku_functions.R")

grid <- 
  R6Class("Grid",
          
          public = list(
            vec = NULL,
            status = NULL,
            df_empty_cases = NULL,
            children = NULL,
            
            initialize = function(vec = NA){
              self$vec <- vec
              self$update_attributes()
            },
            
            # iscomplete = function(){
            #   self$complete <- is_complete(self$vec)
            # },
            
            update_attributes = function(){
              
              self$df_empty_cases <- 
                self$vec %>% 
                is.na() %>% 
                which() %>% 
                tibble(p = .) %>% 
                mutate(n = map(p, ~ authorized_numbers(self$vec, .)),
                       l = map_int(n, length))
              
              if (nrow(self$df_empty_cases) == 0) {
                self$status <- "complete" # Vérifier si full vraie ou full fausse
              } else if (0 %in% self$df_empty_cases$l){
                self$status <- "wrong"
              } else if (1 %in% self$df_empty_cases$l){
                self$status <- "unambiguous"
              } else {
                self$status <- "ambiguous"
              }
            },
            
            fill_unambiguous = function(){
              
              # Must be use when self$status == "unambiguous"
              # Fill empty cases which have only one possibility 
              # Update attributes
              
              df <- 
                self$df_empty_cases %>% 
                filter(l == 1) %>% 
                mutate(n = unlist(n))
              
              self$vec[df$p] <- df$n
              
              self$update_attributes()
            },
            
            print = function(){
              print(self$vec)
              print(plot_matrix(self$vec))
              print(paste("Statut:", self$status))
            }
          )
          )


G1 <- grid$new(V_complete)
G1

G2 <- grid$new(V_almostcomp) 
G2
G2$df_empty_cases
G2$fill_unambiguous()
G2
G2$fill_unambiguous()
G2

G3 <- grid$new(V_impossible)
G3
G3$df_empty_cases
G3$fill_unambiguous()
G3
G3$fill_unambiguous()
G3
G3$df_empty_cases %>% View
G3$fill_unambiguous()
G3
