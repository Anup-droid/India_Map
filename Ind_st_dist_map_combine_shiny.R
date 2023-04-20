# Load required packages
library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)

# Read shapefiles of India
ind_state_shape <- st_read("C:/Shape files 2020/state.shp")
ind_district_shape <- st_read("C:/Shape files 2020/district.shp")

# Define UI for app
ui <- fluidPage(
  titlePanel("Vaccination Coverage in India"),
  tabsetPanel(
    tabPanel("State-wise Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_state", 
                             label = "Select variable:",
                             choices = c("BCG", "ROTA","Penta","HepB","MCV1","MCV2"),
                             selected = "BCG")
               ),
               mainPanel(
                 plotOutput("ind_map_state")
               )
             )
    ),
    tabPanel("District-wise Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_district", 
                             label = "Select variable:",
                             choices = c("BCG", "ROTA","Penta","HepB","MCV1","MCV2"),
                             selected = "BCG")
               ),
               mainPanel(
                 plotOutput("ind_map_district")
               )
             )
    )
  )
)

# Define server logic for app
server <- function(input, output) {
  
  # Generate some random data points
  set.seed(123)
  ind_data <- data.frame(
    OBJECTID = ind_district_shape$OBJECTID,
    STATE = ind_district_shape$STATE,
    BCG = runif(length(ind_district_shape$OBJECTID), 0, 100),
    ROTA = runif(length(ind_district_shape$OBJECTID), 0, 100),
    Penta = runif(length(ind_district_shape$OBJECTID), 0, 100),
    HepB = runif(length(ind_district_shape$OBJECTID), 0, 100),
    MCV1 = runif(length(ind_district_shape$OBJECTID), 0, 100),
    MCV2 = runif(length(ind_district_shape$OBJECTID), 0, 100)
  )
  
  # Merge data with shapefiles
  ind_map_data_state <- merge(ind_state_shape, ind_data, by.x = "OBJECTID", by.y = "OBJECTID")
  ind_map_data_district <- merge(ind_district_shape, ind_data, by.x = "OBJECTID", by.y = "OBJECTID")
  
  # Render state-wise map
  output$ind_map_state <- renderPlot({
    ggplot(ind_map_data_state, aes(fill = !!sym(input$var_state))) +
      geom_sf(color = "black") +
      scale_fill_viridis_b() +
      labs(title = "Coverage of Vaccination in India - State-wise",
           subtitle = paste0(input$var_state, " Coverage"),
           caption = "Source: HMIS - Data upto Feb-2023") +
      theme(title = element_text(face = "bold"),
            legend.position = "left") +
      theme_void()
  })
  
  # Render district-wise map
  output$ind_map_district <- renderPlot({
    ggplot(ind_map_data_district, aes(fill = !!sym(input$var_district))) +
      geom_sf(color = "black") +
      scale_fill_viridis_b() +
      labs(title = "Coverage of Vaccination in India - District-wise",
           subtitle = paste0(input$var_district, " Coverage"),
           caption = "Source: HMIS - Data upto Feb-2023") +
      theme(title = element_text(face = "bold"),
            legend.position = "left") +
      theme_void()
  })}
#Run Shiny app
shinyApp(ui,server)
