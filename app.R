# Valo Mara CS 424 Project 2 Spring '21

library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)

# ----- DATA CLEANUP -----
# read in data
plnt18 <- read.csv("plnt.csv")

# join other columns together
plnt18$OTHER <- rowSums(plnt18[,14:15])
plnt18$OTHER1 <- NULL
plnt18$OTHER2 <- NULL

# join total generation across all energy sources
plnt18$TOTAL_GEN <- rowSums(plnt18[,5:14])

# calculate percentage of each energy source at a plant
plnt18$PERCENT_COAL <- round(plnt18$COAL/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_OIL <- round(plnt18$OIL/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_GAS <- round(plnt18$GAS/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_NUCLEAR <- round(plnt18$NUCLEAR/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_HYDRO <- round(plnt18$HYDRO/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_BIO <- round(plnt18$BIO/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_WIND <- round(plnt18$WIND/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_SOLAR <- round(plnt18$SOLAR/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_GEO <- round(plnt18$GEO/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_OTHER <- round(plnt18$OTHER/plnt18$TOTAL_GEN, digits = 3)

# adding all renewable columns
plnt18$TOTAL_RENEWABLE <- rowSums(plnt18[,9:13])

# adding all non-renewable columns
plnt18$TOTAL_NON_RENEWABLE <- rowSums(plnt18[,c(5:8, 14)])

# calculating percentage of renewable/non-renewable energy generated
plnt18$PERCENT_RENEWABLE <- round(plnt18$TOTAL_RENEWABLE/plnt18$TOTAL_GEN, digits = 3)
plnt18$PERCENT_NON_RENEWABLE <- round(plnt18$TOTAL_NON_RENEWABLE/plnt18$TOTAL_GEN, digits = 3)

# ----- UI -----
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(),
  dashboardBody()
)

# ----- SERVER -----
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
