library(R6)

source("sudoku_functions.R")

grid <- 
  R6Class("Grid",
          
          public = list(
            vec = NULL,
            status = NULL,
            df_empty_cases = NULL,
            children = NULL,
            solutions = NULL,
            original_vec = NULL,
            
            initialize = function(vec = NA){
              
              # Initialization method for vec, df_empty_cases and status attributes
              
              self$vec <- vec
              self$update_attributes()
            },
            
            update_attributes = function(){
              
              # Update df_empty_cases and status attributes
              
              self$df_empty_cases <- 
                self$vec %>% 
                is.na() %>% 
                which() %>% 
                tibble(p = .) %>% 
                mutate(n = map(p, ~ authorized_numbers(self$vec, .)),  # From functions file
                       l = map_int(n, length))
              
              if (nrow(self$df_empty_cases) == 0) {
                if (is_complete(self$vec)) {
                  self$status <- "complete" 
                } else {
                  self$status <- "wrong"
                }
              } else if (0 %in% self$df_empty_cases$l){
                self$status <- "wrong"
              } else if (1 %in% self$df_empty_cases$l){
                self$status <- "unambiguous"
              } else {
                self$status <- "ambiguous"
              }
            },
            
            fill_cases = function(p, n){
              
              # Fill positions specified in vector p with numbers n
              
              self$vec[p] <- n
              invisible(self)
            },
            
            fill_unambiguous = function(){
              
              # Must be use when self$status == "unambiguous"
              # Fill empty cases which have only one possibility 
              # Update attributes after
              
              df <- 
                self$df_empty_cases %>% 
                filter(l == 1) %>% 
                mutate(n = unlist(n))
              
              self$fill_cases(df$p, df$n)
              
              self$update_attributes()
            },
            
            create_children = function(){

              # Must be use when self$status == "ambiguous"
              # Create children when one assumption must be made
              # Children are component of a list in attributes
              # Choose position with the minimal number of possibilities

              df <-
                self$df_empty_cases %>% 
                arrange(l) %>% 
                .[1,]
              
              self$children <- 
                map(as.list(unlist(df$n)), ~ grid$new(self$fill_cases(df$p, .)$vec))
            },
            
            create_tree = function(other){
              
              # Creat the whole tree from the original matrix
              # Each node is an ambiguous matrix
              # Save correct grids as an argument of the original matrix
              
              while (self$status == "unambiguous") {
                self$fill_unambiguous()
              }
              
              if (self$status == "ambiguous") {
                self$create_children()
                for (i in 1:length(self$children)) {
                  self$children[[i]]$create_tree(other)
                }
              }
              
              if (self$status == "complete") {
                other$solutions <- c(other$solutions, list(grid$new(self$vec)))
              }
            },
            
            solve = function(){
              
              # Create whole tree and pick solutions in the original matrix attributes
              
              self$solutions <- list()
              self$original_vec <- self$vec 
              # A changer pour un enfant unique de self et créer l'arbre à partir de celui-ci
              self$create_tree(self)
              
            },
            
            print = function(){
              
              # Print method 
              
              base::print(matrix(self$vec, ncol = 9))
            },
            
            announce_solutions = function(){
              
              # Display all solutions with an introducing sentence
              
              if (is.null(self$solutions)) {
                self$solve()
              }
              
              if (length(self$solutions) == 0) {
                base::print("No solution found")
              } else if (length(self$solutions) == 1) {
                base::print("One solution found:")
                self$solutions[[1]]
              } else {
                base::print(paste(length(self$solutions), "solutions found:"))
                for (i in self$solutions) {
                  i$print()
                }
              }
            },
            
            plot_solution = function(i = 1, fillcolor = "lightblue"){
              
              # Plot solutions with background color for discovered numbers
              
              if (is.null(self$solutions)) {
                self$solve()
              }
              
              if (i <= length(self$solutions))  {
                plot_matrix(self$solutions[[i]]$vec, self$original_vec, fillcolor)
              }
            }
          )
          )


# Tests 

G5 <- grid$new(V_hardcore)
G5$vec
G5$solve() 
G5$announce_solutions()
G5$vec
G5$original_vec
G5$plot_solution()

G6 <- G5$children[[1]]
G6
G6$announce_solutions()
G6$plot_solution()

G7 <- grid$new(V_hardcoremultiple)
G7
G7$solve()
G7$announce_solutions()
G7$plot_solution()
G7$plot_solution(2)
G7$plot_solution(3)

