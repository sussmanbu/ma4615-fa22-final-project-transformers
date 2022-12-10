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


usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

# Load Dataset
us_states <- st_read("../../dataset/cb_2019_us_state_20m/cb_2019_us_state_20m.shp", quiet = TRUE)

not_included <-
  c("Guam", "Commonwealth of the Northern Mariana Islands",
    "American Samoa", "Puerto Rico", "United States Virgin Islands")

us_states <- us_states %>%
  filter(!(NAME %in% not_included))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
          tags$h2("TMAP"),
          tmapOutput(outputId = "tmapMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tmapMap <- renderTmap({
    tm_shape(us_states) +
      tm_view(set.view = c(usaLon, usaLat, usaZoom)) + 
      tm_polygons()
  })

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
