
library(shiny)
library(ggplot2)
library(dplyr)

salaryUI =  function(id) {
  ns =  NS(id)
  tagList(
    fluidRow(
      column(4,
             selectInput(ns("company"), "Company:",
                         choices = NULL, selected = NULL)
      ),
      column(4,
             selectInput(ns("position"), "Position:",
                         choices = NULL, selected = NULL)
      ),
      column(4,
             selectInput(ns("salary_cat"), "Salary Category:",
                         choices = NULL, selected = NULL)
      )
    ),

    # Summary Info
    fluidRow(
      column(12,
             textOutput(ns("filter_info"))
      )
    ),

    # Plots
    fluidRow(
      column(6, plotOutput(ns("salary_histogram"))),
      column(6, plotOutput(ns("exp_boxplot")))
    ),

    # Table
    fluidRow(
      column(12,
             tableOutput(ns("salary_table"))
      )
    )
  )
}

salaryServer =  function(id, salary_data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns =  session$ns

    observe({
      data =  salary_data_reactive()

      companies =  c("All", sort(unique(data$Company)))
      updateSelectInput(session, "company", choices = companies, selected = "All")

      cats =  c("All", sort(unique(data$Salary_Category)))
      updateSelectInput(session, "salary_cat", choices = cats, selected = "All")

      reactive_positions =  reactive({
        req(data)
        filtered =  if (input$company == "All") data else data[data$Company == input$company, ]
        sort(unique(filtered$Position_Standardized))
      })

      observe({
        positions =  c("All", reactive_positions())
        updateSelectInput(session, "position", choices = positions, selected = "All")
      })
    })

    filtered_salary =  reactive({
      data =  salary_data_reactive()

      if (input$company != "All" && !is.null(input$company)) {
        data =  data[data$Company == input$company, ]
      }
      if (input$position != "All" && !is.null(input$position)) {
        data =  data[data$Position_Standardized == input$position, ]
      }
      if (input$salary_cat != "All" && !is.null(input$salary_cat)) {
        data =  data[data$Salary_Category == input$salary_cat, ]
      }

      req(nrow(data) > 0)
      data
    })

    output$filter_info =  renderText({
      tryCatch({
        data =  filtered_salary()
        paste("Showing", nrow(data), "salary records matching your filters.")
      }, error = function(e) {
        "No data matches the selected filters."
      })
    })

    output$salary_histogram =  renderPlot({
      data =  filtered_salary()
      req(nrow(data) > 0)

      ggplot(data, aes(x = Salary)) +
        geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.8) +
        labs(title = "Distribution of Salaries (AMD)", x = "Monthly Salary (AMD)", y = "Count") +
        theme_minimal() +
        scale_x_continuous(labels = scales::comma)
    })

    output$exp_boxplot =  renderPlot({
      data =  filtered_salary()
      req(nrow(data) > 0)
      
  
      data$Experience_Level <- factor(data$Experience_Level, 
                                      levels = c("Entry Level (0 years)", "Junior (0-2 years)", "Mid-Level (2-5 years)", "Senior (5-10 years)", "Expert (10+ years)"))
      
      ggplot(data, aes(x = Experience_Level, y = Salary, fill = Experience_Level)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
        labs(title = "Salary by Experience Level", y = "Salary (AMD)", x = "Experience Level") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    })
    

    output$salary_table =  renderTable({
      data =  filtered_salary()
      req(nrow(data) > 0)

      data %>%
        select(Company, Position_Standardized, Position, Experience_Level, Salary_Category, Salary) %>%
        arrange(desc(Salary)) %>%
        head(20)
    }, striped = TRUE, bordered = TRUE, width = "100%")
  })
}