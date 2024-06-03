library(shiny)
library(ggplot2)
library(shinydashboard)
library(googleVis)
library(shinythemes)
library(leaflet)
library(DT)
library(maps)
library(plotly)
library(leaflet)
shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$img(src = "https://vinuni.edu.vn/wp-content/uploads/2024/04/logo-VInUni_ngang-1.png", height = "30px"),
    titleWidth = 250
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      column(
        3,
        checkboxGroupInput(
          "variables",
          "Choose variables to display:",
          choices = c(
            "Average Cost" = "avg_cost",
            "Median Debt" = "md_debt",
            "Median Earnings" = "md_earnings_10"
          ),
          selected = c("avg_cost", "md_debt", "md_earnings_10")
        ),
        sliderInput(
          "size",
          "Point Size:",
          min = 0.1,
          max = 5,
          value = 2.5,
          step = 0.1
        )
      ),
      column(9, leafletOutput("map"))
    ),
    
    fluidRow(column(
      3, selectInput(
        "schoolType",
        "School Type",
        choices = c(
          "Public" = "1",
          "Private" = "2",
          "For-profit" = "3"
        )
      )
    ), column(
      9,
      htmlOutput("boxPlot"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Rates",
          plotOutput("rates_plot", width = "800px", height = "400px")
        ),
        tabPanel(
          "Degrees",
          plotOutput("degs_plot", width = "800px", height = "400px")
        )
      )
    )),
    fluidRow(
      column(
        3,
        selectizeInput(
          inputId = "school_type",
          label = "School Type",
          choices = c('All', 'Public', 'Private Non-Profit', 'Private For-Profit')
        ),
        sliderInput(
          "pop",
          "School Size",
          min = 0,
          max = 55000,
          value = c(0, 55000),
          step = 5000
        ),
        sliderInput(
          "adm_rate",
          "Rate of Admissions",
          min = 0,
          max = 1,
          value = c(0, 1),
          step = .05
        )
      ),
      column(9, htmlOutput("scatterPlot"))
    ),
    
    
    fluidRow(column(
      3,
      selectizeInput(
        inputId = "density",
        label = "Choose Value to Plot:",
        choices = c(
          "% Students with Loans",
          "% Loan Default Rate",
          "Loan Repayment Rate",
          "Average Family Income",
          "Average Student Debt",
          "Average Earnings after 10 Years"
        )
      )
    ), column(9, plotlyOutput("densityPlot"))),
    
  )
))