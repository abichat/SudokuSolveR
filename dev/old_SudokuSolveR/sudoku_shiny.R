library(shiny)
library(shinymaterial)

source("sudoku_class.R")

choices_grid <- c("-" = NA, "1" = 1, "2" = 2, "3" = 3, "4" = 4,
                  "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9)

my_color <- "indigo"

my_material_dropdown <- function(chnumber){
  material_dropdown(
    input_id = paste0("C", chnumber), label = NULL,
    choices = choices_grid)
}

my_material_column <- function(vec_chnumber){
  vec_chnumber <- as.character(vec_chnumber)
  material_column(width = 1, 
                  my_material_dropdown(vec_chnumber[1]),
                  my_material_dropdown(vec_chnumber[2]),
                  my_material_dropdown(vec_chnumber[3]),
                  br(),
                  my_material_dropdown(vec_chnumber[4]),
                  my_material_dropdown(vec_chnumber[5]),
                  my_material_dropdown(vec_chnumber[6]),
                  br(),
                  my_material_dropdown(vec_chnumber[7]),
                  my_material_dropdown(vec_chnumber[8]),
                  my_material_dropdown(vec_chnumber[9])
  )
}

# Wrap shinymaterial apps in material_page
ui <- material_page(
  nav_bar_fixed = TRUE,
  title = "SudokuSolveR",
  material_side_nav(background_color = "grey lighten-4"),
  # align = "left",
  nav_bar_color = my_color,
  tags$h4("    Fill your grid:"),
  material_row(
    material_column(width = 1),
    material_column(width = 11, align = "center",
      # material_column(width = 1),
      my_material_column(1:9),
      my_material_column(10:18),
      my_material_column(19:27),
      material_column(width = 1),
      my_material_column(28:36),
      my_material_column(37:45),
      my_material_column(46:54),
      material_column(width = 1),
      my_material_column(55:63),
      my_material_column(64:72),
      my_material_column(73:81)
      )
    ),
  br(),
  material_row(
    material_column(align = "center", width = 12,
                    material_button(input_id = "solve", label = "SOLVE",
                                    color = my_color, depth = 5))
  ),
  br(),
  plotOutput(outputId = "plot")
)

server <- function(input, output) {
  
  # rv <- reactiveValues()
  # 
  # observeEvent(input$solve, {
  #   rv$vec <- numeric(length = 81)
  #   for (i in 1:81) {
  #     rv$vec[i] <- input[[paste0("C", i)]]
  #   }
  #   rv$Grid <- grid$new(rv$vec)
  #   rv$Grid$solve()
  # })
  # 
  # output$plot <- renderPlot({
  #   plot_matrix(rv$Grid$vec)
  # })
  
  # Grid <- reactive({
  #   grid$new(rv$vec)
  # })
  
  Grid <- reactive({
    grid$new(1:81)
  })
  
  observeEvent(input$solve, {
    print("Hello")
    vec <- reactive({
      vec <- numeric(length = 81)
      for (i in 1:81) {
        vec[i] <- input[[paste0("C", i)]]
      }
      vec
    })
    print(vec())
    
    Grid <- reactive({
      grid$new(vec())
    })
    print(Grid()) # Ok - s'actualise bien avec solve
    print(Grid()$vec) # Ok - s'actualise bien avec solve
    
  })
  
  output$plot <- renderPlot({
    plot_matrix(Grid()$vec) # Ne s'actualise pas avec solve :/
  })
  
}
shinyApp(ui = ui, server = server)