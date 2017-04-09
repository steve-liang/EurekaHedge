library(shiny)

shinyUI(navbarPage("EurekaHedge Dashboard",
                   
       #Tab 1: Performance Viewer
       tabPanel("Current Summary",
                
                fluidRow(    
                  column(6,
                         plotOutput("index", height = 600), 
                         align = "center"
                  ),
                  column(6,
                         plotOutput("sorted", height = 600), 
                         align = "center"
                  )
                ),
                
                fluidRow(    
                  column(6,
                         sliderInput("TimeSelector", label = h5("Trailing # Month(s)"), min = 0, max = 200, value = 12), 
                         align = "center"
                  ),
                  column(6,
                         sliderInput("MonthSelector", label = h5("# Month(s) Ago"), min = 0, max = 12, value = 0), 
                         align = "center"
                  )
                )
       ),
       
       tabPanel("Correlation Heatmap", 
                plotOutput("correlation", height = 800)        
       )
))



