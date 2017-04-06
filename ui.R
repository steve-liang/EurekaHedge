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
                ),
                
                fluidRow(    
                  column(6,
                         sliderInput("RangeSelector", label = h3("Select Time Range"), min = 0, max = 12, value = 0)
                  ),
                  column(6,
                         sliderInput("TimeSelector", label = h3("Select # Months Ago"), min = 0, max = 12, value = 0)
                  )
                  
                )
       )
)
)



