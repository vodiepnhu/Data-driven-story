library(shiny)
library(ggplot2)
library(googleVis)
library(stringr)
library(scales)
library(DT)
library(maps)
library(leaflet)
library(plotly)

function(input, output) {

  # Create datatable
  output$table <- DT::renderDataTable({
    
    a = which(colnames(undergrad_table) == input$selected) - 1 #Find index of column to sort on based on input
    
    dtoptions = if (a == 2) {
      (list(dom = 'tipr', pageLength = 25, order = list(list(a ,'asc')))) #Sort ascending if admission rate is selected
    } else {
      (list(dom = 'tipr', pageLength = 25, order = list(list(a ,'desc')))) #Otherwise sort by ascending 
    }

    datatable(undergrad_table, options = dtoptions, rownames = FALSE) %>%
      formatStyle(input$selected, color = 'darkslategray',
                  background="darkturquoise", fontWeight='bold') %>% 
      formatCurrency(c('Average Total Cost', 'Average Tuition', 'Median Debt', 'Median Earnings'), digits = 0) %>% 
      formatPercentage(c('Admission Rate', 'Students With Loans', 'Comp Sci Majors', 'Math Majors'), 1) 
  })  
  
  
  # Select and rename columns to create df for user interface density plot
  density_data = undergrad_data %>% select(., school_type, `Median Family Income` = md_fam_inc, `Students on Loans (%)` = pct_loan, 
                                           `Median Debt` = md_debt, `Default Rate (%)` = default_rate, `Repayment Rate` = repay_rate, 
                                           `Median Earnings` = md_earnings_10)
  
  output$densityPlot <- renderPlotly({
    
    school1 <- density_data[which(density_data$school_type == "1"),] #Group by school type
    density1 <- density(school1[[input$density]], na.rm = TRUE)
    
    school2 <- density_data[which(density_data$school_type == "2"),]
    density2 <- density(school2[[input$density]], na.rm = TRUE)
    
    school3 <- density_data[which(density_data$school_type == "3"),]
    density3 <- density(school3[[input$density]], na.rm = TRUE)
    
    # density plot w/ user input and group data by school type
    plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Public',  hoverinfo = 'text', text = 'Public', line = list(color = "#124D8B") , fill = 'tozeroy') %>%
      add_trace(x = ~density2$x, y = ~density2$y, name = 'Private', line = list(color = "#C73537"), hoverinfo = 'text', text = 'Private',  fill = 'tozeroy') %>%
      add_trace(x = ~density3$x, y = ~density3$y, name = 'For-Profit', line = list(color = "#ff9900"), hoverinfo = 'text', text = 'For-Profit',  fill = 'tozeroy') %>%
      layout(title= "<b>Density Plot by School Type</b>", titlefont=list(size=14),
             autosize = F, width = 800, height = 400,
             margin = list(l = 75, r = 75, b = 75, t = 75, pad = 4),
             xaxis = list(title = input$density, rangemode='nonnegative'),
             yaxis = list(title = 'Density', showticklabels = FALSE))
  })
  
  # Group data by state and calcuate average debt, cost, and earnings for each state
  cost_by_state = undergrad_data %>% group_by(., state) %>% summarise(., Debt = round(mean(md_debt, na.rm = TRUE)), 
                                                                      Cost = round(mean(avg_cost, na.rm = TRUE)), 
                                                                      Earnings = round(mean(md_earnings_10, na.rm = TRUE)), 
                                                                      Default = round(mean(default_rate, na.rm = TRUE),2))
  mapStates = map("state", fill = TRUE, plot = FALSE)
  
  names(cost_by_state)[1] <-'state.abb' #Prepare data for mapping by creating column with full state name
  cost_by_state$region <- tolower(state.name[match(cost_by_state$state.abb,  state.abb)])
  cost_by_state = cost_by_state %>% arrange(., region)
  cost_by_state[cost_by_state$state.abb == "DC", 6] = "district of columbia" 
  cost_by_state$labels = cost_by_state$region #Create column to be used for pop-up labels with capitalized state names
  cost_by_state[cost_by_state$state.abb == "WA", 6] = "washington:main" #Rename states to match with names in map() program
  cost_by_state[cost_by_state$state.abb == "MI", 6] = "michigan:south"
  cost_by_state[cost_by_state$state.abb == "NY", 6] = "new york:main"
  cost_by_state[cost_by_state$state.abb == "VA", 6] = "virginia:main"
  cost_by_state[cost_by_state$state.abb == "NC", 6] = "north carolina:main"
  cost_by_state[cost_by_state$state.abb == "MA", 6] = "massachusetts:main"
  cost_by_state = cost_by_state %>% filter(., !(state.abb %in% c("PR", "GU", "VI", "AK", "HI"))) #Remove non-contiguous states and other regions
  rownames(cost_by_state) = cost_by_state$region
  
  cost_by_state$Cost[mapStates$names] #Arrange data according to order in map() program so mapping will correspond
  cost_by_state$cost[mapStates$names[1]]
  cost_by_state[mapStates$names[1],]
  cost_by_state2 = as.data.frame(cost_by_state)
  cost_by_state2$Cost[mapStates$names]
  cost_by_state2[mapStates$names,]
  cost_by_state2[mapStates$names,'Cost']

  cost_by_state2$labels = str_to_title(cost_by_state$labels)
  
  output$maxBox <- renderInfoBox({
    max_value <- max(cost_by_state2[,input$mapval])
    max_state <-
      cost_by_state2$labels[cost_by_state2[,input$mapval]==max_value]
    infoBox(paste("Highest", input$mapval, sep = " "), max_state, dollar(max_value), color = "#ff9900", icon = icon("money"))
  })
  
  output$minBox <- renderInfoBox({
    min_value <- min(cost_by_state2[,input$mapval])
    min_state <-
      cost_by_state2$labels[cost_by_state2[,input$mapval]==min_value]
    infoBox(paste("Lowest", input$mapval, sep = " "), min_state, dollar(min_value), color = "red", icon = icon("money"))
  })
  
  # Create reactive map using leaflet
  output$mymap = renderLeaflet({ 
    palette <- colorBin(c('#fee0d2', #choose appropriate bins based on user input variable
                          '#fcbba1',
                          '#fc9272',
                          '#fb6a4a',
                          '#ef3b2c',
                          '#cb181d',
                          '#a50f15'), 
                        domain = cost_by_state2[mapStates$names, input$mapval],
                        na.color = NA,
                        pretty = TRUE, 
                        bins = 6)
    
    labels = sprintf( #Create pop-up labels with state name and average value based on user input variable 
      "<strong>%s</strong><br/>%s",
      cost_by_state2[mapStates$names,'labels'], dollar(cost_by_state2[mapStates$names, input$mapval])
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% #Create heat map of the US based on user input variable
      addPolygons(data = mapStates, fillColor = ~palette(cost_by_state2[mapStates$names, input$mapval]), fillOpacity = 0.6,         
                  color = "white",       
                  weight = 1.5, highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels, #Add labels which will change based on user input variable
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend(position = 'topleft', #Add legend which will change based on user input variable
                pal = palette, 
                values = cost_by_state2[mapStates$names, input$mapval],
                opacity = 0.6,
                labFormat = labelFormat(prefix = "$"),
                title = paste("Median", input$mapval, sep = " "))
  })
  

  
  # Select columns from data to be used in scatterplot
  scatter_data = undergrad_data[,c("college", "school_type", "adm_rate", "population", "avg_cost", "md_earnings_10")]
  
  scatter_data$school_type = as.character(scatter_data$school_type)
  
  # Prepare data for googleVis by creating separate avg_cost columns for each school type
  # Create tooltip columns corresponding to each so school name will pop up on data point
  # Create style columns corresponding to each to select custom style options (point border and color) 
  scatter_data[["Public"]] = ifelse(scatter_data[["school_type"]]=="1", scatter_data[["avg_cost"]], NA)
  scatter_data[["Public.tooltip"]] = scatter_data[["college"]]
  scatter_data[["Public.style"]] = 'stroke-color: #124D8B'
  scatter_data[["Private"]] <- ifelse(scatter_data[["school_type"]]=="2", scatter_data[["avg_cost"]], NA)
  scatter_data[["Private.tooltip"]] = scatter_data[["college"]]
  scatter_data[["Private.style"]] = 'stroke-color: #C73537'
  scatter_data[["For-Profit"]] = ifelse(scatter_data[["school_type"]]=="3", scatter_data[["avg_cost"]], NA)
  scatter_data[["For-Profit.tooltip"]] = scatter_data[["college"]]
  scatter_data[["For-Profit.style"]] = 'stroke-color: #ff9900'
  scatter_data[["avg_cost"]] = NULL

  filtered_scatter = reactive({  #Create reactive element to filter data based on user inputs
    scatter_data2 = scatter_data %>%
      filter(.,
        population >= input$pop[1], #Filter based on selected population range
        population <= input$pop[2],
        adm_rate >= input$adm_rate[1], #Filter based on selected admission rate range
        adm_rate <= input$adm_rate[2]
      )
    
    if (input$school_type == 'All') { #Filter based on selected school type (All, Public, Private, Non-Profit)
      (scatter_data2)
    } else if (input$school_type == 'Public') {
      (scatter_data2 %>% filter(., school_type == "1"))
    } else if (input$school_type == 'Private Non-Profit') {
      (scatter_data2 %>%  filter(., school_type == "2"))
    } else {
      (scatter_data2 %>%  filter(., school_type == "3"))
    }

  })

  output$scatterPlot = renderGvis({  #Create scatterplot
    filtered_scatter2 = filtered_scatter() %>% select(., -1, -2, -3, -4)
    
    my_options <- list(width="800px", height="400px",
                       title="Comparing College Cost & Earnings",
                       hAxis="{title:'Earnings', format: '$#,###'}", #Format axes as dollar amounts
                       vAxis="{title:'Cost per Year', format: '$#,###'}",
                       dataOpacity = 0.4)
    gvisScatterChart(filtered_scatter2, options = my_options)
  })

}