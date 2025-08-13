library(shiny)
library(dplyr)
library(ggplot2)

analyticsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, selectInput(ns("city_filter"), "City:", choices = NULL)),
      column(3, selectInput(ns("bedroom_filter"), "Bedrooms:", choices = NULL)),
      column(3, selectInput(ns("bed_filter"), "Beds:", choices = NULL)),
      column(3, uiOutput(ns("rating_slider_ui")))
    ),
    uiOutput(ns("price_slider_ui")),

    fluidRow(
      column(6, plotOutput(ns("price_histogram"), height = "400px")),
      column(6, plotOutput(ns("rating_bar")), height = "400px")
    )
  )
}

analyticsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      df <- data()

      updateSelectInput(session, "city_filter",
                        choices = c("All", sort(unique(df$City.Category[df$City.Category != "Other Armenia"]))),
                        selected = "All")

      updateSelectInput(session, "bedroom_filter",
                        choices = c("All", sort(unique(df$Bedrooms))),
                        selected = "All")

      updateSelectInput(session, "bed_filter",
                        choices = c("All", sort(unique(df$Beds))),
                        selected = "All")

      output$rating_slider_ui <- renderUI({
        df <- data()
        ratings <- df$Overall.Rating
        ratings <- ratings[!is.na(ratings)]
        
        if (length(ratings) == 0) return(NULL)
        
        sliderInput(ns("rating_filter"),
                    "Overall Rating:",
                    min = floor(min(ratings, na.rm = TRUE)*10)/10,
                    max = ceiling(max(ratings, na.rm = TRUE)*10)/10,
                    value = c(floor(min(ratings)*10)/10, ceiling(max(ratings)*10)/10),
                    step = 0.1)
      })
      
    })

    output$price_slider_ui <- renderUI({
      df <- data() %>%
        filter(Max.Guests > 0) %>%
        mutate(PricePerGuest = TTM.Avg.Daily.Rate / Max.Guests) %>%
        filter(!is.na(PricePerGuest), is.finite(PricePerGuest))

      min_val <- floor(min(df$PricePerGuest, na.rm = TRUE))
      max_val <- ceiling(max(df$PricePerGuest, na.rm = TRUE))

      sliderInput(ns("price_per_guest"),
                  "Price per Guest ($):",
                  min = min_val, max = max_val,
                  value = c(min_val, max_val))
    })

    filtered_data <- reactive({
      req(input$price_per_guest)
      df <- data() %>%
        filter(Max.Guests > 0) %>%
        mutate(PricePerGuest = TTM.Avg.Daily.Rate / Max.Guests)

      if (input$city_filter != "All") {
        df <- df %>% filter(City.Category == input$city_filter)
      }
      if (input$bedroom_filter != "All") {
        df <- df %>% filter(Bedrooms == input$bedroom_filter)
      }
      if (input$bed_filter != "All") {
        df <- df %>% filter(Beds == input$bed_filter)
      }
      df <- df %>%
        filter(!is.na(Overall.Rating),
               Overall.Rating >= input$rating_filter[1],
               Overall.Rating <= input$rating_filter[2])

      df %>%
        filter(!is.na(PricePerGuest), is.finite(PricePerGuest),
               PricePerGuest >= input$price_per_guest[1],
               PricePerGuest <= input$price_per_guest[2])
    })

    output$price_histogram <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)

      ggplot(df, aes(x = TTM.Avg.Daily.Rate)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "white") +
        labs(title = "Price Distribution",
             x = "Daily Rate ($)",
             y = "Frequency") +
        theme_minimal()
    })

    output$rating_bar <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)

      
      plot_data <- df %>%
        mutate(Room.Type = recode(Room.Type, hotel_room = "Hotel room")) %>%
        group_by(Room.Type) %>%
        summarise(AvgRating = mean(Overall.Rating, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(AvgRating))

      if (nrow(plot_data) == 0) {
        ggplot() +
          annotate("text", x = 1, y = 1, label = "No data available", size = 6) +
          theme_void() +
          theme(panel.background = element_rect(fill = "white"))
      } else {
        ggplot(plot_data, aes(x = reorder(Room.Type, -AvgRating), y = AvgRating)) +
          geom_col(fill = "orange", alpha = 0.7) +
          labs(title = "Average Rating by Room Type",
               x = "Room Type",
               y = "Average Overall Rating") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
  })
}