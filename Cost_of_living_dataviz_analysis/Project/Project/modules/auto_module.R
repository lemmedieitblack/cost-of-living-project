autoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Auto Data Visualizations - Armenia"),
    
    sidebarLayout(
      sidebarPanel(
        
        tabsetPanel(id = ns("filterTabs"), 
                    tabPanel("Color Filter",
                             selectInput(ns("colorInput"), "Select Color:", choices = NULL)
                    ),
                    tabPanel("Motor Type Filter",
                             selectInput(ns("motorInput"), "Select Motor Type:", choices = NULL)
                    ),
                    tabPanel("Year Filter",
                             selectInput(ns("yearInput"), "Select Year:", choices = NULL)
                    ),
                    tabPanel("Brand Filter",
                             selectInput(ns("brandInput"), "Select Brand:", choices = NULL)
                    )
        )
      ),
      
      mainPanel(
        tabsetPanel(
          id = ns("graphTabs"), 
          
          tabPanel("Price by Color", value = "priceByColor", plotOutput(ns("colorBoxplot"))), 
          tabPanel("Mean Price by Motor Type", value = "priceByMotor",
                   verbatimTextOutput(ns("motorPriceText")),
                   plotOutput(ns("motorPricePlot"))),
          tabPanel("Mean Price by Year", value = "priceByYear", 
                   verbatimTextOutput(ns("yearPriceText")),
                   plotOutput(ns("yearPricePlot"))),
          tabPanel("Mean Price by Brand", value = "priceByBrand", 
                   verbatimTextOutput(ns("brandPriceText")),
                   plotOutput(ns("brandPricePlot")),
                   br(),
                   h4("Cars from Selected Brand"),
                   dataTableOutput(ns("brandTable")))
        )
      )
    )
  )
}

# Server
autoServer <- function(id, data_reactive) { 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    observe({
      req(data_reactive()) 
      current_data <- data_reactive() 
      
      updateSelectInput(session, "colorInput", choices = sort(unique(current_data$color)))
      updateSelectInput(session, "motorInput", choices = sort(unique(current_data$motor)))
      updateSelectInput(session, "yearInput", choices = sort(unique(current_data$year[current_data$year <= 2025])))
      updateSelectInput(session, "brandInput", choices = sort(unique(current_data$brand)))
    })
    
    
    observe({
      req(input$graphTabs)  
      updateTabsetPanel(session, ns("filterTabs"), 
                        selected = switch(input$graphTabs,
                                          "priceByColor" = "Color Filter",
                                          "priceByMotor" = "Motor Type Filter",
                                          "priceByYear"  = "Year Filter",
                                          "priceByBrand" = "Brand Filter"))
    })
    
    
    
    observeEvent(input$graphTabs, {
      selected_graph_tab <- input$graphTabs
      cat("Graph tab changed to:", selected_graph_tab, "\n")
      
      selected_filter <- switch(
        selected_graph_tab,
        "priceByColor" = "Color Filter",
        "priceByMotor" = "Motor Type Filter",
        "priceByYear"  = "Year Filter",
        "priceByBrand" = "Brand Filter"
      )
      
      if (!is.null(selected_filter)) {
        updateTabsetPanel(
          session = session,
          inputId = "filterTabs",  
          selected = selected_filter
        )
      }
    })
    
    
    filtered_color_data <- reactive({
      req(data_reactive())
      data_reactive() %>% filter(color == input$colorInput)
    })
    filtered_motor_data <- reactive({
      req(data_reactive())
      data_reactive() %>% filter(motor == input$motorInput)
    })
    filtered_year_data  <- reactive({
      req(data_reactive())
      data_reactive() %>% filter(year == input$yearInput)
    })
    filtered_brand_data <- reactive({
      req(data_reactive())
      data_reactive() %>% filter(brand == input$brandInput)
    })
    
    
    output$colorBoxplot <- renderPlot({
      req(filtered_color_data()) 
      ggplot(filtered_color_data(), aes(x = color, y = price)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot of Price for", input$colorInput, "Cars"), x = "Color", y = "Price (USD)")
    })
    
    output$motorPriceText <- renderPrint({
      req(filtered_motor_data())
      mean_price <- filtered_motor_data() %>% summarise(mean_price = mean(price, na.rm = TRUE)) %>% pull(mean_price)
      cat("Mean Price for Motor Type", input$motorInput, "is", round(mean_price, 0), "USD")
    })
    
    output$motorPricePlot <- renderPlot({
      req(data_reactive()) # Access data_reactive here for the overall plot
      data_reactive() %>%
        filter(!is.na(price), !is.na(motor), str_trim(motor) != "") %>%
        group_by(motor) %>%
        summarise(mean_price = mean(price, na.rm = TRUE)) %>%
        ggplot(aes(x = reorder(motor, mean_price), y = mean_price, fill = motor == input$motorInput)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "skyblue")) +
        labs(title = "Mean Price by Motor Type", x = "Motor Type", y = "Mean Price (USD)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$yearPriceText <- renderPrint({
      req(filtered_year_data())
      mean_price <- filtered_year_data() %>% summarise(mean_price = mean(price, na.rm = TRUE)) %>% pull(mean_price)
      cat("Mean Price for Year", input$yearInput, "is", round(mean_price, 0), "USD")
    })
    
    output$yearPricePlot <- renderPlot({
      req(data_reactive()) 
      data_reactive() %>%
        filter(!is.na(year), !is.na(price), year <= 2025) %>%
        group_by(year) %>%
        summarise(mean_price = mean(price, na.rm = TRUE)) %>%
        ggplot(aes(x = factor(year), y = mean_price, fill = year == input$yearInput)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "darkorange", "FALSE" = "lightgray")) +
        labs(title = "Mean Price by Year of Manufacture", x = "Year", y = "Mean Price (USD)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$brandPriceText <- renderPrint({
      req(filtered_brand_data())
      mean_price <- filtered_brand_data() %>% summarise(mean_price = mean(price, na.rm = TRUE)) %>% pull(mean_price)
      cat("Mean Price for Brand", input$brandInput, "is", round(mean_price, 0), "USD")
    })
    
    output$brandPricePlot <- renderPlot({
      req(data_reactive()) 
      data_reactive() %>%
        filter(!is.na(brand), !is.na(price)) %>%
        group_by(brand) %>%
        summarise(mean_price = mean(price, na.rm = TRUE)) %>%
        ggplot(aes(x = reorder(brand, mean_price), y = mean_price, fill = brand == input$brandInput)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("TRUE" = "darkorange", "FALSE" = "lightgray")) +
        coord_flip() +
        labs(title = "Mean Price by Car Brand", x = "Brand", y = "Mean Price (USD)") +
        theme_minimal()
    })
    
    output$brandTable <- renderDataTable({
      req(filtered_brand_data())
      filtered_brand_data() %>%
        select(car, price, year, motor, color, type) %>%
        arrange(desc(price))
    })
  })
}