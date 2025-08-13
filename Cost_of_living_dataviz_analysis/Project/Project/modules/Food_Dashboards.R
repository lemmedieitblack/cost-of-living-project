
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)

foodUI <- function(id) {
  ns <- NS(id)
  tabPanel("Food Price Dashboard",
           tabsetPanel(
             tabPanel("1. Avg Price Over Time", plotOutput(ns("avg_over_time"))),
             tabPanel("2. Volatility", plotOutput(ns("volatility"))),
             tabPanel("3. Expensive Categories", plotOutput(ns("expensive_categories"))),
             tabPanel("4. Monthly Basket Cost", plotOutput(ns("basket_cost"))),
             tabPanel("5. Affordability vs Wage", plotOutput(ns("affordability"))),
             tabPanel("6. Heatmap", plotOutput(ns("heatmap"))),
             tabPanel("7. Regional Trends",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(ns("selected_region"), "Select a Region:",
                                      choices = NULL, 
                                      selected = NULL)
                        ),
                        mainPanel(
                          plotOutput(ns("regional_trend"))
                        )
                      )
             )
           )
  )
}


foodServer <- function(id, food_data_reactive) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      req(food_data_reactive())
      validate(need(food_data_reactive(), "Food price data is unavailable."))
      food_data_reactive()    
    })

    observe({
      req(data())
      regions <- sort(unique(data()$admin1))
      updateSelectInput(session, "selected_region",
                        choices = regions,
                        selected = regions[1])
    })

    # 1. Average Price Over Time
    output$avg_over_time <- renderPlot({
      req(data())
      data() %>%
        group_by(date) %>%
        summarise(avg_price = mean(usdprice, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = date, y = avg_price)) +
        geom_line(color = "darkgreen", size = 0.8) +
        labs(title = "Average Food Price Over Time",
             x = "Date", y = "Average Price (USD)") +
        theme_minimal()
    })

    # 2. Volatility (Top 10 Most Volatile Commodities)
    output$volatility <- renderPlot({
      req(data())
      data() %>%
        group_by(commodity) %>%
        summarise(sd_price = sd(usdprice, na.rm = TRUE), n = n(), .groups = "drop") %>%
        filter(n > 20) %>%
        slice_max(sd_price, n = 10) %>%
        ggplot(aes(x = reorder(commodity, sd_price), y = sd_price)) +
        geom_col(fill = "tomato") +
        coord_flip() +
        labs(title = "Top 10 Most Volatile Food Items",
             x = "Commodity", y = "Price Std Dev (USD)") +
        theme_minimal()
    })

    # 3. Most Expensive Categories
    output$expensive_categories <- renderPlot({
      req(data())
      data() %>%
        group_by(category) %>%
        summarise(avg_usd = mean(usdprice, na.rm = TRUE), .groups = "drop") %>%
        slice_max(avg_usd, n = 10) %>%
        ggplot(aes(x = reorder(category, avg_usd), y = avg_usd)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        labs(title = "Top 10 Most Expensive Food Categories",
             x = "Category", y = "Average Price (USD)") +
        theme_minimal()
    })

    # 4. Monthly Food Basket Cost
    output$basket_cost <- renderPlot({
      req(data())
      basket_items <- c("Bread", "Rice", "Milk", "Eggs", "Sunflower Oil", "Potatoes")
      quantities <- tibble::tibble(
        commodity = basket_items,
        quantity = c(8, 2, 10, 30, 1.5, 5)
      )

      data() %>%
        filter(commodity %in% basket_items) %>%
        group_by(date, commodity) %>%
        summarise(avg_price = mean(usdprice, na.rm = TRUE), .groups = "drop") %>%
        left_join(quantities, by = "commodity") %>%
        mutate(item_cost = avg_price * quantity) %>%
        group_by(date) %>%
        summarise(total_basket = sum(item_cost, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = date, y = total_basket)) +
        geom_line(color = "darkorange", size = 1) +
        labs(title = "Cost of Monthly Food Basket",
             subtitle = "Bread, Rice, Milk, Eggs, Sunflower Oil, Potatoes",
             x = "Date", y = "Total Cost (USD)") +
        theme_minimal()
    })

    # 5. Affordability vs Minimum Wage
    output$affordability <- renderPlot({
      req(data())
      wage_usd <- 75000 / 390  

      basket_items <- c("Bread", "Rice", "Milk", "Eggs", "Sunflower Oil", "Potatoes")
      quantities <- tibble::tibble(
        commodity = basket_items,
        quantity = c(8, 2, 10, 30, 1.5, 5)
      )

      cost_data <- data() %>%
        filter(commodity %in% basket_items) %>%
        group_by(date, commodity) %>%
        summarise(avg_price = mean(usdprice, na.rm = TRUE), .groups = "drop") %>%
        left_join(quantities, by = "commodity") %>%
        mutate(item_cost = avg_price * quantity) %>%
        group_by(date) %>%
        summarise(total_basket = sum(item_cost, na.rm = TRUE), .groups = "drop") %>%
        mutate(afford_ratio = wage_usd / total_basket)

      ggplot(cost_data, aes(x = date, y = afford_ratio)) +
        geom_line(color = "blue", size = 1) +
        labs(title = "Food Affordability (Wage ÷ Basket)",
             subtitle = "How many food baskets can minimum wage buy?",
             x = "Date", y = "Baskets per Min Wage") +
        theme_minimal()
    })

    # 6. Heatmap: Monthly Average Prices
    output$heatmap <- renderPlot({
      req(data())
      data() %>%
        mutate(year = format(date, "%Y"),
               month = format(date, "%m")) %>%
        group_by(year, month) %>%
        summarise(mean_price = mean(usdprice, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = month, y = year, fill = mean_price)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c(option = "B", name = "Avg Price (USD)") +
        labs(title = "Monthly Average Food Prices",
             x = "Month", y = "Year") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # 7. Regional Trends
    output$regional_trend <- renderPlot({
      req(data(), input$selected_region)
      data() %>%
        filter(price > 0,
               admin1 == input$selected_region,
               !is.na(admin1),
               date >= as.Date("2018-01-01")) %>%
        group_by(date) %>%
        summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = date, y = avg_price)) +
        geom_line(color = "darkblue", size = 1) +
        labs(title = paste("Food Price Trends in", input$selected_region),
             x = "Date", y = "Average Price (AMD)") +
        theme_minimal()
    })
  })
}