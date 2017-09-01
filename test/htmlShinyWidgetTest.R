# htmlWidgetShinyTest
library(shiny)
library(ndtv)
library(network)

data(short.stergm.sim)  # load datasets to use
data(toy_epi_sim)
# Define server logic required to draw a interactive plot
server <- function(input, output) {
  nets <-
    list(short.stergm.sim = short.stergm.sim, toy_epi_sim = toy_epi_sim)
  
  
  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    net <- nets[[input$networkSelection]]
    render.d3movie(net, output.mode = 'htmlWidget')
  })
}

# Define UI for application that renders static ndtv-d3 view of networks
ui <- fluidPage(# Application title
  titlePanel("ndtv shinyWidget test"),
  
  # Sidebar with a selector for picking which network to render
  sidebarLayout(
    sidebarPanel(
      p(
        'This is a test of the ndtv-d3 htmlWidget from the ndtv package. Should be able to play, pause, scrub timeline, zoom in (mousewheel), pan (drag), show tooltips (click), and hilight neighbors (double click) '
      ),
      selectInput(
        'networkSelection',
        'Select  example network:',
        c('short.stergm.sim', 'toy_epi_sim')
      )
    ),
    
    # Show the interactive network plot
    mainPanel(
      h2('ndtv-d3 interactive network animation'),
      tags$style(HTML(".tooltip {opacity: 1}")),
      # stop boostrap css from messing up the tooltip in the widget
      ndtv:::ndtvAnimationWidgetOutput("netPlot")
    )
  ))

shinyApp(ui = ui, server = server)