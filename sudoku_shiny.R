library(shiny)
library(shinymaterial)

choices_grid <- c("-" = NA, "1" = 1, "2" = 2, "3" = 3, "4" = 4,
                  "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9)

my_material_dropdown <- function(chnumber){
  material_dropdown(
    input_id = chnumber, label = NULL,
    choices = choices_grid)
}

my_material_column <- function(vecchnumber){
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
  title = "SudokuSolver",
  nav_bar_color = "light-blue",
  tags$h4("Fill your grid:"),
  material_row(
    material_column(width = 0),
    my_material_column(as.character(1:9)),
    my_material_column(as.character(10:18)),
    my_material_column(as.character(19:27)),
    material_column(width = 1),
    my_material_column(as.character(28:36)),
    my_material_column(as.character(37:45)),
    my_material_column(as.character(46:54)),
    material_column(width = 1),
    my_material_column(as.character(55:63)),
    my_material_column(as.character(64:72)),
    my_material_column(as.character(73:81))
    ),
  br(),
  material_row(
    material_column(align = "center", width = 12,
                    material_button(input_id = "solve", label = "SOLVE",
                                    color = "light-blue", depth = 5))
  ),
  br()
)

server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)