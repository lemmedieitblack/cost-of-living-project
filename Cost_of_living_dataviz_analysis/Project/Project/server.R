
server <- function(input, output, session) {
  rentalMapServer("rental_map", rental_data_reactive)
  analyticsServer("analytics", rental_data_reactive)
  visualsServer("visuals", rental_data_reactive)
  salaryServer("salary", salary_data_reactive)
  housesServer("houses", houses_data_reactive)
  foodServer("food", food_data_reactive)
  autoServer("auto", auto_data_reactive)
}