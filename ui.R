library(shiny)
library(ggplot2)
library(shinydashboard)
library(googleVis)
library(shinythemes)
library(leaflet)
library(DT)
library(maps)
library(plotly)

shinyUI(dashboardPage(
  skin = "blue",
  header = dashboardHeader(
    title = tagList(
      shiny::tags$img(src = "logo.png", height = "50px", style = "padding: 10px;")
    ),
    titleWidth = 250
  ),
  sidebar = dashboardSidebar(disable = TRUE),  # Provide an empty sidebar and disable it
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItem(tabName = "scatter",
            fluidRow(
              column(3,  # Inputs column
                     selectizeInput(inputId = "school_type", 
                                    label = "School Type",
                                    choices = c('All','Public', 'Private Non-Profit', 'Private For-Profit')),
                     sliderInput("pop", "School Size",
                                 min = 0, max = 55000, value = c(0, 55000), step = 5000),
                     sliderInput("adm_rate", "Rate of Admissions",
                                 min = 0, max = 1, value = c(0, 1), step = .05),
                     
                     sliderInput("year", "Year",
                                 min = 1996, max = 2022, value = c(1996, 2022), step = 1),
              ),
              column(9,  # Output column
                     htmlOutput("scatterPlot")  # Ensure the plot output ID matches the server code
              )
            ),
      tabItem(
              tabName = "boxplot",
              fluidRow(
                column(
                  12, 
                  div(
                    style = "height: 200px; background-color: #f0f0f0; border: 2px dashed #ccc; display: flex; align-items: center; justify-content: center;", 
                    h4("Box plot - Content under development...")
                  )
                )
              ),
      
      tabItem(tabName = "data",
              fluidRow(
              column(3,  # Inputs column
                      selectizeInput(inputId = "density", #add dropdown
                                     label = "Choose Value to Plot:",
                                     choices = c("Median Family Income", "Students on Loans (%)", "Median Debt",
                                                 "Default Rate (%)", "Repayment Rate", "Median Earnings")),
                     sliderInput("year", "Year",
                                 min = 1996, max = 2022, value = c(1996, 2022), step = 1),
              ),
              
              column(9, # Output column
                     plotlyOutput("densityPlot")))),  #add density chart
    
      tabItem(tabName = "map",
            fluidRow(column(4, selectizeInput(inputId = "mapval", #add dropdown menu
                                              label = "Choose Value to Map:",
                                              choices = c('Cost','Debt', 'Earnings'),
                                              selected = 'Cost')),
                     column(4, infoBoxOutput("maxBox")), tags$style("#maxBox {width:300px; height:50px;}"),
                     column(4, infoBoxOutput("minBox")), tags$style("#minBox {width:300px;}")),
            fluidRow(leafletOutput("mymap"))) #add map

      ))
  )
))
