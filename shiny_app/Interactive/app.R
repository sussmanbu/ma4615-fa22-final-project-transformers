# Loading the required packages
library(dplyr)
library(shiny)
library(leaflet)
library(tmap)
library(sf)
library(tidyverse)

# Loading the Datasets and files
load("abortionDataClean.RData")
load("StateAff.RData")
us_states <- st_read("cb_2019_us_state_20m/cb_2019_us_state_20m.shp", quiet = TRUE)

# Initial setup for map
usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3
epsg_us <- 2163

colours <- c("#00AEF3", "#808080", "#E81B23")

not_included <-
  c("Guam", "Commonwealth of the Northern Mariana Islands",
    "American Samoa", "Puerto Rico", "United States Virgin Islands")

us_states <- 
  us_states %>%
  filter(!(NAME %in% not_included))

us_states <- 
  st_transform(us_states, epsg_us)

aff_year <- 
  stateAff_data_clean %>% 
  filter(year == 1976)

us_states_aff <- 
  inner_join(us_states, stateAff_data_clean, by = c("STUSPS" = "state")) %>% 
  select(NAME, STUSPS, Affiliation, geometry, year)

# Initial setup for bar graphs
clickedState <- NULL
gData <-
   abortionDataClean %>% 
   inner_join(stateAff_data_clean, by = c("state","year"))

# Define UI for application
ui <- 
  fluidPage(
    titlePanel("Political Affiliation of each State by Year"),
    
    absolutePanel(
      top = 1,
      right = 20,
      sliderInput(
        "year",
        "Year:",
        ticks = FALSE,
        min = 1988,
        max = 2016,
        value = 1988,
        step = 4,
        animate = animationOptions(
          interval = 5000,
          loop = TRUE
          )
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
        splitLayout(
          cellWidths = c("50%", "50%"),
          plotOutput(outputId = "abortionPlot"),
          plotOutput(outputId = "pregPlot"))
        ), 
      width = 100
      ),
  
    mainPanel(
      tags$h2(""),
      tags$h2(""),
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"), 
          plotOutput(outputId = "abortionAgePlot"), 
          plotOutput(outputId = "pregAgePlot"))
        ),
      width = 100
      )
    )

server <- 
  function(input, output) {
    
    data <- 
      reactive({
        us_states_aff %>% 
        filter(year == input$year)
      })
  
    graphData <- 
      reactive({
        gData %>% 
        filter(year == input$year)
      })
  
    output$tmapMap <- 
      renderTmap({
        tm_shape(data()) +
        tm_view(set.view = c(usaLon, usaLat, usaZoom)) + 
        tm_polygons(col = "Affiliation", title = "Political Affiliation by State", palette = colours)
      })
    
    output$abortionPlot <- 
      renderPlot({
        data <- graphData() 
        
        #Output error message for missing data
        validate(
          need( nrow(data) > 0, "No data for plot")
        )
        
        data %>%
          ggplot() +
          stat_summary(aes(x = reorder(state, AbortionRate), y = AbortionRate, fill = as.factor(Affiliation)), geom = "bar") + 
          coord_flip() + 
          scale_fill_manual(values = c('#00214d', '#7c7b78', '#e91515')) + 
          labs(title = 'Average Abortion Rate by State Affiliation', x = 'State\n', y = '\nAverage Abortion Rate', fill = "Affliation") + 
          ggthemes::theme_economist() +
          theme(axis.text.y = element_text(size = 7), axis.title = element_text(size = 10), legend.position = 'right', legend.text = element_text(size = 8))
        })  
    
    output$abortionAgePlot <- 
      renderPlot({
        data <- graphData()
          
        if(!is.null(input$tmapMap_shape_click)){
          clickedState <- input$tmapMap_shape_click$id
          
          #Output error message for missing data
          validate(
            need( nrow(data) > 0, "No data for plot")
          )
          
          stateName <- tolower(substr(clickedState, 1, nchar(clickedState) - 2))
        
          data %>% 
            filter(tolower(stateFull) == stateName) %>% 
            ggplot() + 
            stat_summary(aes(x = Age, y = AbortionRate, fill = as.factor(Age)), geom = "bar") + 
            coord_flip() + 
            scale_fill_viridis_d(option = "cividis") + 
            labs(title = 'Average Abortion Rate by Age', x = 'Age\n', y = '\nAverage Abortion Rate', fill = "Age") + 
            ggthemes::theme_economist()
        }
        
        if(!is.null(clickedState)) {
          data <- data %>% 
            filter(tolower(stateFull) == stateName)
          
          #Output error message for missing data
          validate(
            need( nrow(data) > 0, "No data for plot")
          )
          
          data %>% 
            ggplot() + 
            stat_summary(aes(x = Age, y = AbortionRate, fill = as.factor(Age)), geom = "bar") + 
            coord_flip() + 
            scale_fill_viridis_d(option = "cividis") + 
            labs(title = 'Average Abortion Rate by Age', x = 'Age\n', y = '\nAverage Abortion Rate', fill = "Age") + 
            ggthemes::theme_economist()
        }})
    
    output$pregPlot <- 
      renderPlot({
        data <- graphData() 
        
        #Output error message for missing data
        validate(
          need( nrow(data) > 0, "No data for plot")
        )
        
        data %>%
          ggplot() + 
          stat_summary(aes(x = reorder(state, PregnancyRate),  y = PregnancyRate, fill = as.factor(Affiliation)), geom = "bar") + 
          coord_flip() + 
          scale_fill_manual(values = c('#00214d', '#7c7b78', '#e91515')) + 
          labs(title = 'Average Pregnancy Rate by State Affiliation', x = 'State\n', y = '\nAverage Pregnancy Rate', fill = "Affliation") + 
          ggthemes::theme_economist()+
          theme(axis.text.y = element_text(size = 7), axis.title = element_text(size = 10), legend.position = 'right', legend.text = element_text(size = 8))
        })
    
    output$pregAgePlot <- 
      renderPlot({
        data <- graphData() 
        
        if(!is.null(input$tmapMap_shape_click)){
          clickedState <- input$tmapMap_shape_click$id
          
          stateName <- tolower(substr(clickedState, 1, nchar(clickedState) - 2))
          
          print(stateName)
          
          data <- data %>% 
            filter(tolower(stateFull) == stateName)
          
          #Output error message for missing data
          validate(
            need( nrow(data) > 0, "No data for plot")
          )
          
          data %>% 
            ggplot() + 
            stat_summary(aes(x = Age, y = PregnancyRate, fill = as.factor(Age)), geom = "bar") + 
            coord_flip() + 
            scale_fill_viridis_d(option = "cividis") + 
            labs(title = 'Average Pregnancy Rate by Age', x = 'Age\n', y = '\nAverage Pregnancy Rate', fill = "Age") + 
            ggthemes::theme_economist()
        }
        
        if(!is.null(clickedState)){
          #Output error message for missing data
          validate(
            need( nrow(data) > 0, "No data for plot")
          )
          
          data %>% 
            filter(tolower(stateFull) == stateName) %>% 
            ggplot() + 
            stat_summary(aes(x = Age, y = PregnancyRate, fill = as.factor(Age)), geom = "bar") + 
            coord_flip() + 
            scale_fill_viridis_d(option = "cividis") + 
            labs(title = 'Average Pregnancy Rate by Age', x = 'Age\n', y = '\nAverage Pregnancy Rate', fill = "Age") + 
            ggthemes::theme_economist()
        }})
    }

# Run the application 
shinyApp(ui = ui, server = server)