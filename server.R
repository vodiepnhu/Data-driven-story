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

function(input, output) {
  # Create datatable
  output$table <- DT::renderDataTable({
    a = which(colnames(undergrad_table) == input$selected) - 1 #Find index of column to sort on based on input
    
    dtoptions = if (a == 2) {
      (list(
        dom = 'tipr',
        pageLength = 25,
        order = list(list(a , 'asc'))
      )) #Sort ascending if admission rate is selected
    } else {
      (list(
        dom = 'tipr',
        pageLength = 25,
        order = list(list(a , 'desc'))
      )) #Otherwise sort by ascending
    }
    
    datatable(undergrad_table, options = dtoptions, rownames = FALSE) %>%
      formatStyle(
        input$selected,
        color = 'darkslategray',
        background = "darkturquoise",
        fontWeight = 'bold'
      ) %>%
      formatCurrency(
        c(
          'Average Total Cost',
          'Average Tuition',
          'Median Debt',
          'Median Earnings'
        ),
        digits = 0
      ) %>%
      formatPercentage(c(
        'Admission Rate',
        'Students With Loans',
        'Comp Sci Majors',
        'Math Majors'
      ),
      1)
  })
  
  
  # Select columns from data to be used in scatterplot
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
  
  
  #Boxplot
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
      labs(title = "Rates", x = "Values", y = "") +
      scale_y_discrete(labels = labels_map) +
      theme_minimal()
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
      labs(title = "Degrees", x = "Values", y = "") +
      scale_y_discrete(labels = labels_map) +
      theme_minimal()
  })
  
  # Density plot
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
  
  
  # Map
  # First, ensure the data is correctly converted to numeric where expected
  undergrad_data2 <- undergrad_data %>%
    mutate(
      md_debt = as.numeric(as.character(md_debt)),
      avg_cost = as.numeric(as.character(avg_cost)),
      md_earnings_10 = as.numeric(as.character(md_earnings_10)),
      default_rate = as.numeric(as.character(default_rate))
    )
  # Calculate summary statistics
  cost_by_state <- undergrad_data %>%
    group_by(state) %>%
    summarise(
      Debt = round(mean(md_debt, na.rm = TRUE)),
      Cost = round(mean(avg_cost, na.rm = TRUE)),
      Earnings = round(mean(md_earnings_10, na.rm = TRUE)),
      Default = round(mean(default_rate, na.rm = TRUE), 2)
    ) %>%
    ungroup()
  
  # Fix the labels
  cost_by_state$labels <- state.name[match(cost_by_state$state, state.abb)]
  
  # Handle the mapping
  states_map <- map_data("state") # Load US states map data
  # Merge your data with the map data
  map_data <- merge(states_map, cost_by_state, by.x = "region", by.y = "labels")
  
  output$mymap <- renderLeaflet({
    leaflet(data = map_data) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(
        fillColor = ~colorBin(palette = c('#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15'),
                              domain = Cost, bins = 6)(Cost),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        label = ~paste(labels, ": $", Cost),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(pal = colorBin(palette = c('#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15'), domain = Cost, bins = 6),
                values = ~Cost, title = "Median Cost", opacity = 1.0, labFormat = labelFormat(prefix = "$"))
  })
  
  # Update infoBox implementation to new standards
  output$maxBox <- renderInfoBox({
    max_value <- max(cost_by_state[[input$mapval]], na.rm = TRUE)
    max_state <- cost_by_state$labels[which.max(cost_by_state[[input$mapval]])]
    infoBox(
      title = paste("Highest", input$mapval),
      subtitle = max_state,
      value = format(max_value, big.mark = ",", scientific = FALSE),
      color = "red"
    )
  })
  
  output$minBox <- renderInfoBox({
    min_value <- min(cost_by_state[[input$mapval]], na.rm = TRUE)
    min_state <- cost_by_state$labels[which.min(cost_by_state[[input$mapval]])]
    infoBox(
      title = paste("Lowest", input$mapval),
      subtitle = min_state,
      value = format(min_value, big.mark = ",", scientific = FALSE),
      color = "green"
    )
  })
  

}