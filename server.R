library(shiny)
library(ggplot2)
library(googleVis)
library(stringr)
library(scales)
library(DT)
library(maps)
library(leaflet)
library(plotly)
library(dplyr)
library(forcats)
library(tidyr)

undergrad_data<- read.csv("/Users/nhuvo/Desktop/MyShiny/undergrad.csv")
function(input, output) {
  #1. Map
  mymap_data <- undergrad_data
  
  output$map <- renderLeaflet({
    selected_vars <- input$variables
    base_map <- mymap_data %>%
      filter(!is.na(long), !is.na(lat)) %>%
      leaflet() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addTiles()
    base_map <- base_map %>%
      addControl(html = '<h5 style="margin: 2px; padding: 1px; color: #555;font-weight: bold; background-color: white;">Educational Data: Cost, Debt, and Earnings by School Type</h5>', position = "topright")
    if ("avg_cost" %in% selected_vars) {
      base_map <- base_map %>%
        addCircleMarkers(
          ~long, ~lat, radius = ~avg_cost / 10000 * input$size,
          fillColor = "orange", fillOpacity = ~avg_cost / max(mymap_data$avg_cost, na.rm = TRUE),
          stroke = TRUE, weight = 1, color = "orange",
          popup = ~paste0(college, "<br>Average Cost: $", avg_cost)
        )
    }
    
    if ("md_debt" %in% selected_vars) {
      base_map <- base_map %>%
        addCircleMarkers(
          ~long, ~lat, radius = ~md_debt / 10000 * input$size,
          fillColor = "red", fillOpacity = ~md_debt / max(mymap_data$md_debt, na.rm = TRUE),
          stroke = TRUE, weight = 1, color = "red",
          popup = ~paste0(college, "<br>Median Debt: $", md_debt)
        )
    }
    
    if ("md_earnings_10" %in% selected_vars) {
      base_map <- base_map %>%
        addCircleMarkers(
          ~long, ~lat, radius = ~md_earnings_10 / 10000 * input$size,
          fillColor = "green", fillOpacity = ~md_earnings_10 / max(mymap_data$md_earnings_10, na.rm = TRUE),
          stroke = TRUE, weight = 1, color = "green",
          popup = ~paste0(college, "<br>Median Earnings (10 years): $", md_earnings_10)
        )
    }
    
    base_map
  })
  
  #2. Boxplot
  boxplot_data = undergrad_data
  
  # convert to numeric
  boxplot_data[] <- lapply(boxplot_data, function(x)
    as.numeric(as.character(x)))
  
  # get rete and deg group
  rates <- c("adm_rate", "default_rate", "repay_rate", "pct_25k")
  degs <- c("comp_deg", "eng_deg", "engtech_deg", "math_deg", "sci_deg")
  
  # label map
  labels_map <- c(
    adm_rate = "Admission Rate",
    pct_25k = "Percent Earning Over 25K",
    default_rate = "Default Rate",
    repay_rate = "Repayment Rate",
    comp_deg = "Completion Degree",
    eng_deg = "Engineering Degree",
    engtech_deg = "Engineering Tech Degree",
    math_deg = "Mathematics Degree",
    sci_deg = "Science Degree"
  )
  
  #Plot rates_plot
  
  output$rates_plot <- renderPlot({
    filtered_data <- boxplot_data %>%
      filter(school_type == input$schoolType) %>%
      pivot_longer(cols = rates,
                   names_to = "variable",
                   values_to = "value")
    
    ggplot(filtered_data, aes(x = value, y = fct_reorder(variable, value))) +
      geom_boxplot(fill = "skyblue", orientation = "y") +
      labs(title = "Comparative Analysis of Admission, Default, and Repayment Rates",
           x = "Values", y = "") +
      scale_y_discrete(labels = labels_map) +
      theme_minimal()+
      theme(plot.title = element_text(face = "bold"))
  })
  #Plot degs_plot
  output$degs_plot <- renderPlot({
    filtered_data <- boxplot_data %>%
      filter(school_type == input$schoolType) %>%
      pivot_longer(cols = degs,
                   names_to = "variable",
                   values_to = "value")
    
    ggplot(filtered_data, aes(x = value, y = fct_reorder(variable, value))) +
      geom_boxplot(fill = "pink", orientation = "y") +
      labs(title = "Distribution of Degrees in Different Disciplines",
           x = "Values", y = "") +
      scale_y_discrete(labels = labels_map) +
      theme_minimal()+
      theme(plot.title = element_text(face = "bold"))
  })
  
  # 3. Scatterplot
  scatter_data = undergrad_data[, c("college",
                                    "school_type",
                                    "adm_rate",
                                    "population",
                                    "avg_cost",
                                    "md_earnings_10")]
  
  scatter_data$school_type = as.character(scatter_data$school_type)
  
  # Prepare data for googleVis by creating separate avg_cost columns for each school type
  # Create tooltip columns corresponding to each so school name will pop up on data point
  # Create style columns corresponding to each to select custom style options (point border and color)
  scatter_data[["Public"]] = ifelse(scatter_data[["school_type"]] == "1", scatter_data[["avg_cost"]], NA)
  scatter_data[["Public.tooltip"]] = scatter_data[["college"]]
  scatter_data[["Public.style"]] = 'stroke-color: #00FF00; stroke-opacity: 0.9; fill-color: #00FF00; fill-opacity: 0.6'
  scatter_data[["Private"]] <- ifelse(scatter_data[["school_type"]] == "2", scatter_data[["avg_cost"]], NA)
  scatter_data[["Private.tooltip"]] = scatter_data[["college"]]
  scatter_data[["Private.style"]] = 'stroke-color: #red; stroke-opacity: 0.9; fill-color: #C73537; fill-opacity: 0.6'
  scatter_data[["For-Profit"]] = ifelse(scatter_data[["school_type"]] ==
                                          "3", scatter_data[["avg_cost"]], NA)
  scatter_data[["For-Profit.tooltip"]] = scatter_data[["college"]]
  scatter_data[["For-Profit.style"]] = 'stroke-color: #FFFF00; stroke-opacity: 0.9; fill-color: #FFFF00; fill-opacity: 0.6'
  scatter_data[["avg_cost"]] = NULL
  
  filtered_scatter = reactive({
    #Create reactive element to filter data based on user inputs
    scatter_data2 = scatter_data %>%
      filter(
        .,
        population >= input$pop[1],
        #Filter based on selected population range
        population <= input$pop[2],
        adm_rate >= input$adm_rate[1],
        #Filter based on selected admission rate range
        adm_rate <= input$adm_rate[2]
      )
    
    if (input$school_type == 'All') {
      #Filter based on selected school type (All, Public, Private, Non-Profit)
      (scatter_data2)
    } else if (input$school_type == 'Public') {
      (scatter_data2 %>% filter(., school_type == "1"))
    } else if (input$school_type == 'Private Non-Profit') {
      (scatter_data2 %>%  filter(., school_type == "2"))
    } else {
      (scatter_data2 %>%  filter(., school_type == "3"))
    }
    
  })
  
  output$scatterPlot = renderGvis({
    #Create scatterplot
    filtered_scatter2 = filtered_scatter() %>% select(., -1, -2, -3, -4)
    my_options <- list(
      width = "800px",
      height = "400px",
      title = "Comparing College Cost & Earnings",
      hAxis = "{title:'Earnings', format: '$#,###'}",
      #Format axes as dollar amounts
      vAxis = "{title:'Cost per Year', format: '$#,###'}",
      dataOpacity = 0.4,
      series = "{0: {color: '#00FF00'}, 1: {color: '#C73537'}, 2: {color: '#FFFF00'}}"
    )
    gvisScatterChart(filtered_scatter2, options = my_options)
  })
  
  # 4. Density plot
  # Prepare Dataset
  density_data = undergrad_data %>% select(
    .,
    school_type,
    `% Students with Loans` = pct_loan,
    `% Loan Default Rate` = default_rate,
    `Loan Repayment Rate` = repay_rate,
    `Average Family Income` = md_fam_inc,
    `Average Student Debt` = md_debt,
    `Average Earnings after 10 Years` = md_earnings_10
  )
  density_data$`Loan Repayment Rate` <- as.numeric(gsub("%", "", density_data$`Loan Repayment Rate`)) / 100
  
  output$densityPlot <- renderPlotly({
    school1 <- density_data[which(density_data$school_type == "1"), ] # group by school type
    density1 <- density(school1[[input$density]], na.rm = TRUE) # Determine density plot x-value
    
    school2 <- density_data[which(density_data$school_type == "2"), ]
    density2 <- density(school2[[input$density]], na.rm = TRUE)
    
    school3 <- density_data[which(density_data$school_type == "3"), ]
    density3 <- density(school3[[input$density]], na.rm = TRUE)
    
    # Use plotly to create density plot
    plot_ly(
      x = ~ density1$x,
      y = ~ density1$y,
      type = 'scatter',
      mode = 'lines',
      name = 'Public',
      hoverinfo = 'text',
      text = 'Public',
      line = list(color = "#00FF00"),
      fill = 'tozeroy',
      fillcolor = 'rgba(0, 255, 0, 0.5)'
    ) %>%
      add_trace(
        x = ~ density2$x,
        y = ~ density2$y,
        name = 'Private',
        line = list(color = "red"),
        hoverinfo = 'text',
        text = 'Private',
        fill = 'tozeroy',
        fillcolor = 'rgba(255, 0, 0, 0.5)'
      ) %>%
      add_trace(
        x = ~ density3$x,
        y = ~ density3$y,
        name = 'For-Profit',
        line = list(color = "#FFFF00"),
        hoverinfo = 'text',
        text = 'For-Profit',
        fill = 'tozeroy',
        fillcolor = 'rgba(255, 255, 0, 0.5)'
      ) %>%
      layout(
        title = "<b>Distribution of Income and Debt by School Type<b>",
        titlefont = list(size = 13),
        autosize = F,
        width = 800,
        height = 400,
        margin = list(
          l = 75,
          r = 75,
          b = 75,
          t = 75,
          pad = 4
        ),
        xaxis = list(title = input$density, rangemode = 'nonnegative'),
        yaxis = list(title = 'Density', showticklabels = FALSE),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
}