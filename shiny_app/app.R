# app.R
# Run with: shiny::runApp("shiny_app")
# Deploy:   rsconnect::deployApp("shiny_app")

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
