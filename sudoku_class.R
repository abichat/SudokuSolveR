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
            
            fill_cases = function(p, n){
              self$vec[p] <- n
              invisible(self)
            },
            
            fill_unambiguous = function(){
              
              # Must be use when self$status == "unambiguous"
              # Fill empty cases which have only one possibility 
              # Update attributes
              
              df <- 
                self$df_empty_cases %>% 
                filter(l == 1) %>% 
                mutate(n = unlist(n))
              
              self$fill_cases(df$p, df$n)
              
              self$update_attributes()
              
              if (self$status == "complete") {
                return(self$vec)
              }
            },
            
            create_children = function(){

              # Must be use when self$status == "ambiguous"

              df <-
                self$df_empty_cases %>%
                arrange(l) %>%
                .[1,]

              vec_temp <- self$vec
              self$children <- 
                map(as.list(unlist(df$n)), ~ grid$new(self$fill_cases(df$p, .)$vec))
            },
            
            solve_only_unambiguous = function(){
              while (self$status == "unambiguous") {
                self$fill_unambiguous()
              }
              
              if (self$status == "complete") {
                return(self$vec)
              }
            },
            
            print = function(){
              base::print(self$vec)
              # base::print(plot_matrix(self$vec))
              base::print(paste("Statut:", self$status))
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
# G3$fill_unambiguous()
# G3
# G3$fill_unambiguous()
# G3
# G3$df_empty_cases %>% View
# G3$fill_unambiguous()
# G3
G3$solve_only_unambiguous()
G3

G4 <- grid$new(V_realgrid)
G4
G4$solve_only_unambiguous()
G4



G5 <- grid$new(V_hardcore)
G5$solve_only_unambiguous()
G5

G5$create_children()
G5$children
G5$children[[1]]

G6 <- G5$children[[1]]
G6$solve_only_unambiguous()


# df <- G5$df_empty_cases %>% arrange(l) %>% .[1,]
# df$p
# df$n
# 
# G5$fill_cases(df$p, 4)$vec
# (G5$fill_cases(df$p, 4))$vec
# grid$new((G5$fill_cases(df$p, 4))$vec)
# 
# G5$fill_cases(df$p, 4)
# G5$vec
# 
# 
# 
# L <- map(as.list(unlist(df$n)), ~ grid$new(G5$fill_cases(df$p, .)$vec))
# 
