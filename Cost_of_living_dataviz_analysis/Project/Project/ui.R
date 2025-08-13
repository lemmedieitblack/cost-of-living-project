ui <- fluidPage(
  titlePanel("Cost of Living & Salary in Armenia Insights"),

  tabsetPanel(
    tabPanel("Rental Map View", rentalMapUI("rental_map")),
    tabPanel("Rental Analytics", analyticsUI("analytics")),
    tabPanel("Rental Visuals", visualsUI("visuals")),
    tabPanel("Salary Insights", salaryUI("salary")),
    tabPanel("Houses for Sale", housesUI("houses")),
    tabPanel("Auto Market", autoUI("auto")),
    foodUI("food")  
  )
)