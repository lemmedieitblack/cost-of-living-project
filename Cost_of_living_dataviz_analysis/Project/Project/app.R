source("global.R")

source("modules/rental_map_module.R")
source("modules/salary_module.R")
source("modules/visuals_module.R")
source("modules/analytics_module.R")
source("modules/auto_module.R")
source("modules/houses_module.R")  

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)