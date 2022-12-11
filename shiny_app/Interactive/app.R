#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(leaflet)
library(tmap)
library(sf)
library(tidyverse)


usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

colours <- c("#00AEF3", "#808080", "#E81B23")

# For TMAP
us_states <- st_read("cb_2019_us_state_20m/cb_2019_us_state_20m.shp", quiet = TRUE)

not_included <-
  c("Guam", "Commonwealth of the Northern Mariana Islands",
    "American Samoa", "Puerto Rico", "United States Virgin Islands")

us_states <- us_states %>%
  filter(!(NAME %in% not_included))

epsg_us <- 
  2163

us_states <- 
  st_transform(us_states, epsg_us)

load("StateAff.RData")

aff_year <- stateAff_data_clean %>% filter(year == 1976)

us_states_aff <- inner_join(us_states, stateAff_data_clean, by = c("STUSPS" = "state")) %>% 
  select(NAME, STUSPS, Affiliation, geometry, year)

# For the other two graphs
load("abortionDataClean.RData")
 gData <-
   abortionDataClean %>% 
   inner_join(stateAff_data_clean, by = "state")
 gData <-
   gData %>% 
   filter(year.x == year.y)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Political Affiliation of each State by Year"),
  
  absolutePanel(
    top = 1,
    right = 20,
    selectInput(
      "year",
      "Year:",
      ticks = FALSE,
      min = 1988,
      max = 2016,
      value = 1988,
      step = 4,
      animate = animationOptions(
        interval = 5000,
        loop = TRUE)
      )
    ),
  
  mainPanel(
    tags$h2(""),
    tags$h2(""),
    tmapOutput(outputId = "tmapMap"),
    width = 100
    ),
  
  mainPanel(
    tags$h2(""),
    tags$h2(""),
    
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "abortionPlot"), plotOutput(outputId = "pregPlot"))
    ), width = 100
  )
  )

server <- function(input, output) {
  
  data <- 
    reactive({
      us_states_aff %>% 
      filter(year == input$year)
      
  })
  
  graphData <- 
    reactive({
        gData %>% 
        filter(year.x == input$year)
    })
  
  output$tmapMap <- renderTmap({
    tm_shape(data()) +
      tm_view(set.view = c(usaLon, usaLat, usaZoom)) + 
      tm_polygons(col = "Affiliation", title = "Political Affiliation by State", palette = colours)
  })

    output$abortionPlot <- renderPlot({
      graphData() %>% 
        ggplot() + 
        stat_summary(
          aes(
            x = reorder(state, AbortionRate), 
            y = AbortionRate, 
            fill = as.factor(Affiliation)
            ), 
          geom = "bar") + 
        coord_flip() + 
        scale_fill_viridis_d(option = "cividis") + 
        labs(title = 'Average Abortion Rate by State Affiliation', x = 'State\n', y = '\nAverage Abortion Rate', fill = "Affliation") + 
        ggthemes::theme_economist() +
        theme(axis.text.y = element_text(size = 7), axis.title = element_text(size = 10), legend.position = 'right', legend.text = element_text(size = 8))
      
     })
    
    output$pregPlot <- renderPlot({
      graphData() %>% 
        ggplot() + 
        stat_summary(
          aes(
            x = reorder(state, PregnancyRate), 
            y = PregnancyRate, 
            fill = as.factor(Affiliation)
          ), 
          geom = "bar") + 
        coord_flip() + 
        scale_fill_viridis_d(option = "cividis") + 
        labs(title = 'Average Pregnancy Rate by State Affiliation', x = 'State\n', y = '\nAverage Pregnancy Rate', fill = "Affliation") + 
        ggthemes::theme_economist() +
        theme(axis.text.y = element_text(size = 7), axis.title = element_text(size = 10), legend.position = 'right', legend.text = element_text(size = 8))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
