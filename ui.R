# SELF-ASSEMBLING NETWORKS
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Main title
  titlePanel("Self-Assembling Networks"),
  
  # Load MathJax
  withMathJax(),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      
      sliderInput(
        "PopulationSize",
        "Population Size:",
        min = 2,
        max = 20,
        value = 6,
        step = 1
      ), 
      
      radioButtons(
        "PerfectCommunication",
        "Perfect Communication:",
        choiceNames = list("True", "False"),
        choiceValues = list("Perfect", "Imperfect"),
        selected = "Imperfect"
      ), 
      
      sliderInput(
        "RoundsOfPlay",
        "Number of Rounds:",
        min = 10,
        max = 100,
        value = 20,
        step = 1
      ),
      
      p(actionButton("runSimulation", "Run Simulation"), align = "center")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      ndtv:::ndtvAnimationWidgetOutput("netPlot")
      )
    
  )
  
))
