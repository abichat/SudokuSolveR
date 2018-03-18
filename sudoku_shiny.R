library(shiny)
library(shinymaterial)

choices_grid <- c("-" = NA, "1" = 1, "2" = 2, "3" = 3, "4" = 4,
                  "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9)

my_color <- "indigo"

my_material_dropdown <- function(chnumber){
  material_dropdown(
    input_id = chnumber, label = NULL,
    choices = choices_grid)
}

my_material_column <- function(vecchnumber){
  vecchnumber <- as.character(vecchnumber)
  material_column(width = 1, 
                  my_material_dropdown(vecchnumber[1]),
                  my_material_dropdown(vecchnumber[2]),
                  my_material_dropdown(vecchnumber[3]),
                  br(),
                  my_material_dropdown(vecchnumber[4]),
                  my_material_dropdown(vecchnumber[5]),
                  my_material_dropdown(vecchnumber[6]),
                  br(),
                  my_material_dropdown(vecchnumber[7]),
                  my_material_dropdown(vecchnumber[8]),
                  my_material_dropdown(vecchnumber[9])
  )
}

# Wrap shinymaterial apps in material_page
ui <- material_page(
  nav_bar_fixed = TRUE,
  title = "SudokuSolver",
  material_side_nav(background_color = "grey lighten-4"),
  # align = "left",
  nav_bar_color = my_color,
  tags$h4("Fill your grid:"),
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
  br()
)

server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)