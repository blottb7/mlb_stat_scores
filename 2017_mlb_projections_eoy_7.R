library(shiny)

#load data

#ui
ui <- fluidPage(
  
  #LAYOUT
  verticalLayout(
    
    #SELECT INPUT
    #number of teams
    sliderInput(
      inputId = "n_teams", label = "Number of Fantasy Teams", min = 8, max = 24, value = 12, step = 1
    ),
    
    #starting catchers per team
    numericInput(
      inputId = "starting_catchers", label = "starting catchers per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting first basemen per team
    numericInput(
      inputId = "starting_first_basemen", label = "starting first basemen per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting second basemen per team
    numericInput(
      inputId = "starting_second_basemen", label = "starting second basemen per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting third basemen per team
    numericInput(
      inputId = "starting_third_basemen", label = "starting third basemen per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting shortstops per team
    numericInput(
      inputId = "starting_shortstops", label = "starting shortstops per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting outfielders per team
    numericInput(
      inputId = "starting_outfielders", label = "starting outfielders per team", value = 5, min = 0, max = 9, step = 1
    ),
    
    #starting middle infielders per team
    numericInput(
      inputId = "starting_middle_infielders", label = "starting middle infielders per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting corner infielders per team
    numericInput(
      inputId = "starting_corner_infielders", label = "starting corner infielders per team", value = 1, min = 0, max = 3, step = 1
    ),
    
    #starting designated hitters per team
    numericInput(
      inputId = "starting_designated_hitters", label = "starting designated hitters/utilty per team", value = 1, min = 0, max = 3, step = 1
    )
    
  ),
  #MAIN PANEL
  mainPanel()
)

#server
server <- function() {
  
}

#shiny App
shinyApp(ui, server)
