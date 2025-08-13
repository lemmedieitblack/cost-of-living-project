

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(corrplot)
library(tidyr)
library(stringr)

visualsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Exploratory Visualizations"),
    fluidRow(
      column(6, plotOutput(ns("box_price_room"), height = "400px")),
      column(6, plotOutput(ns("scatter_price_rating"), height = "400px"))
    ),
    fluidRow(
      column(6, plotOutput(ns("hist_occupancy"), height = "400px")),
      column(6, plotOutput(ns("box_rating_aspects"), height = "400px"))
    ),
    leafletOutput(ns("circle_map"), height = "500px")
  )
}

visualsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    library(ggplot2)
    library(leaflet)
    library(dplyr)
    library(tidyr)
    library(stringr)

    prepared_data <- reactive({
      req(data())
      data() %>%
        filter(Max.Guests > 0) %>%
        mutate(
          PricePerGuest = TTM.Avg.Daily.Rate / Max.Guests,
          OccupancyRate = as.numeric(as.character(TTM.Occupancy.Rate))
        )
    })

    # 1. Boxplot: Price per Guest by Room Type (Labels Cleaned)
    output$box_price_room <- renderPlot({
      df <- prepared_data()
      req(nrow(df) > 0)

      # Clean up Room.Type labels by replacing underscores and capitalizing
      df_plot <- df %>%
        mutate(Room.Type = str_replace_all(Room.Type, "_", " ") %>% str_to_title())

      ggplot(df_plot, aes(x = Room.Type, y = PricePerGuest)) +
        geom_boxplot(fill = "lightblue", alpha = 0.8) +
        scale_y_log10(labels = scales::dollar) +
        labs(
          title = "Price per Guest by Room Type (Log Scale)",
          y = "Price per Guest ($)",
          x = "Room Type"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # 2. Scatter: Price vs Rating (Labels Cleaned)
    output$scatter_price_rating <- renderPlot({
      df <- prepared_data()
      req(nrow(df) > 0)

      # Clean up City.Category labels
      df_plot <- df %>%
        mutate(City.Category = str_replace_all(City.Category, "_", " "))

      ggplot(df_plot, aes(x = TTM.Avg.Daily.Rate, y = Overall.Rating, color = City.Category)) +
        geom_point(alpha = 0.7, size = 2) +
        geom_smooth(method = 'lm', se = TRUE, color = "red", linetype = "dashed") +
        scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
        scale_color_brewer(type = "qual", palette = "Set1", name = "City") +
        labs(title = "Price vs Rating by City",
             x = "Price ($)",
             y = "Overall Rating") +
        theme_minimal()
    })

    # 3. Histogram (No labels to change)
    output$hist_occupancy <- renderPlot({
      df <- prepared_data()
      req(nrow(df) > 0)
      req(!is.null(df$OccupancyRate) && !all(is.na(df$OccupancyRate)))

      ggplot(df, aes(x = OccupancyRate)) +
        geom_histogram(bins = 30, fill = "lightgreen", color = "white") +
        labs(title = "Occupancy Rate Distribution",
             x = "Occupancy Rate (%)",
             y = "Frequency") +
        theme_minimal()
    })

    # 4. Boxplot: Rating Aspects (Labels Cleaned)
    output$box_rating_aspects <- renderPlot({
      df <- prepared_data()
      req(nrow(df) > 0)

      rating_cols <- names(df)[grepl("Rating$", names(df))]
      if (length(rating_cols) == 0 || !"Listing.Name" %in% names(df)) {
        plot.new(); text(0.5, 0.5, "No rating data available", col = "gray", cex = 1.2); return()
      }

      ratings_long <- df %>%
        select(Listing.Name, all_of(rating_cols)) %>%
        pivot_longer(
          cols = all_of(rating_cols),
          names_to = "Aspect",
          values_to = "Rating"
        ) %>%
        # Clean up Aspect labels by replacing dots with spaces
        mutate(Aspect = str_replace_all(Aspect, "\\.", " "))

      ratings_long <- ratings_long[!is.na(ratings_long$Rating), ]
      if (nrow(ratings_long) == 0) {
        plot.new(); text(0.5, 0.5, "No valid ratings to display", col = "gray", cex = 1.2); return()
      }

      ggplot(ratings_long, aes(x = reorder(Aspect, -Rating), y = Rating, fill = Aspect)) +
        geom_boxplot(alpha = 0.7, show.legend = FALSE, outlier.alpha = 0.5) +
        labs(title = "Rating Aspects Comparison",
             x = "Aspect",
             y = "Rating") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # 5. Circle Map (No labels to change)
    output$circle_map <- renderLeaflet({
      df <- prepared_data()
      req(nrow(df) > 0)

      df <- df %>%
        mutate(
          Longitude = as.numeric(as.character(Longitude)),
          Latitude = as.numeric(as.character(Latitude))
        ) %>%
        filter(!is.na(Longitude), !is.na(Latitude))

      req(nrow(df) > 0)

      lng <- mean(df$Longitude, na.rm = TRUE)
      lat <- mean(df$Latitude, na.rm = TRUE)

      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = lng, lat = lat, zoom = 12) %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = ~sqrt(OccupancyRate) * 2,
          color = "blue",
          fillOpacity = 0.5,
          popup = ~paste0(
            "<b>", Listing.Name, "</b><br>",
            "Price: $", TTM.Avg.Daily.Rate, "<br>",
            "Occupancy: ", round(OccupancyRate, 1), "%"
          )
        )
    })
  })
}