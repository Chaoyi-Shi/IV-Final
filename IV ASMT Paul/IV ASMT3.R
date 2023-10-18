library("shiny")
library("leaflet")
library('dplyr')
library("ggplot2")
library("ggiraph")

industry_data <- read.csv("area_data.csv", header = TRUE)
##################
# USER INTERFACE #
##################

ui <- navbarPage(
  id = 'mypage',
  title = 'Employment condition in Victoria'
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {

  
}








#############
# RUN SHINY #
#############

shinyApp(ui, server)
