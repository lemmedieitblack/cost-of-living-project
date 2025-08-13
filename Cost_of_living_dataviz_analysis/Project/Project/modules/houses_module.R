
housesServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    observe({
      df <- data()
      updateSelectInput(session, "district", choices = c("All", sort(levels(df$district))), selected = "All")
      updateSelectInput(session, "rooms", choices = c("All", sort(unique(df$num_rooms))), selected = "All")
      updateSelectInput(session, "building", choices = c("All", sort(levels(df$building_type))), selected = "All")
    })


    output$price_slider <- renderUI({
      df <- data()
      vals <- df$price
      sliderInput(ns("price_range"), "Price Range ($):", min = floor(min(vals, na.rm = TRUE)), max = ceiling(max(vals, na.rm = TRUE)), value = c(floor(min(vals, na.rm = TRUE)), ceiling(max(vals, na.rm = TRUE))))
    })


    filtered <- reactive({
      req(input$price_range)
      df <- data()
      if (input$district != "All") { df <- filter(df, district == input$district) }
      if (input$rooms != "All") { df <- filter(df, num_rooms == input$rooms) }
      if (input$building != "All") { df <- filter(df, building_type == input$building) }
      df <- filter(df, price >= input$price_range[1], price <= input$price_range[2])
      df
    })

    output$hist_price <- renderPlot({
      df <- filtered()
      req(nrow(df) > 0)
      ggplot(df, aes(x = price)) + geom_histogram(bins = 30, fill = "skyblue", color = "white") + labs(title = "Price Distribution", x = "Price ($)", y = "Count") + theme_minimal()
    })

    output$scatter_area_price <- renderPlot({
      df <- filtered()
      req(nrow(df) > 0)
      ggplot(df, aes(x = area, y = price)) + geom_point(alpha = 0.6, color = "steelblue") + geom_smooth(method = 'lm', se = TRUE, color = "red", linetype = "dashed") + labs(title = "Price vs Area", x = "Area (sq.m)", y = "Price ($)") + theme_minimal()
    })

    output$price_by_rooms <- renderPlot({
      df <- filtered()
      req(nrow(df) > 0)
      df <- df %>% mutate(num_rooms = as.factor(num_rooms))
      ggplot(df, aes(x = num_rooms, y = price, fill = num_rooms)) + geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) + scale_fill_brewer(palette = "Blues", guide = FALSE) + labs(title = "Price Distribution by Number of Rooms", x = "Number of Rooms", y = "Price ($)") + theme_minimal()
    })


    # 4. Bar Plot: Average Price by District (Legend Removed)
    output$avg_price_by_district <- renderPlot({
      df <- filtered()
      req(nrow(df) > 0)

      avg_price <- df %>%
        group_by(district) %>%
        summarise(avg_price = mean(price, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_price))

      ggplot(avg_price, aes(x = reorder(district, -avg_price), y = avg_price, fill = avg_price)) +
        geom_col(show.legend = FALSE) + 
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(title = "Average Price by District",
             x = "District",
             y = "Avg Price ($)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })


    output$price_area_by_building <- renderPlot({
      df <- filtered()
      req(nrow(df) > 0)
      ggplot(df, aes(x = area, y = price, color = building_type)) + geom_point(alpha = 0.7, size = 2) + scale_color_brewer(type = "qual", palette = "Set1", name = "Building Type") + labs(title = "Price vs Area (Colored by Building Type)", x = "Area (sq.m)", y = "Price ($)") + theme_minimal() + guides(color = guide_legend(override.aes = list(size = 3)))
    })
  })
}