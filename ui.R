library(shiny)

shinyUI(navbarPage("EurekaHedge Dashboard",

  #Tab 1: Performance Viewer
  tabPanel("Current Summary",

      fluidRow(    
        column(6,
               plotOutput("index", height = 600)
               ),
        column(6,
               plotOutput("sorted", height = 600)
               )
        )
      
      # fluidRow(    
      #   column(6,
      #          style = "background-color:blue", div(style = "height:500px;")),
      #   column(6,
      #          style = "background-color:red", div(style = "height:500px;"))
      # )
      
  )
)
)



