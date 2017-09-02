# SELF-ASSEMBLING NETWORKS
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(

  # CSS for visual stying
  includeCSS("www/style.css"),

  # Main title
  titlePanel("Self-Assembling Networks"),

  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Styling to remove "small" tick marks from sliders
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),

      # Introduction text
      p("This model explores how a social network might self-assemble from the ritualization of the decisions of individual agents."),

      # Instructions text
      p("To explore the model, pick your simulation parameters, then click 'Run Simulation'."),

      # Slider for population size
      sliderInput(
        "PopulationSize",
        "Population Size:",
        min = 2,
        max = 20,
        value = 6,
        step = 1
      ),

      # Radio buttons for wether communication is perfect or imperfect
      radioButtons(
        "PerfectCommunication",
        "Perfect Communication:",
        choiceNames = list("True", "False"),
        choiceValues = list("Perfect", "Imperfect"),
        selected = "Imperfect"
      ),

      # Slider for the edge threshold
      sliderInput(
        "EdgeThreshold",
        "Edge Threshold:",
        min = 0,
        max = 6,
        value = 1,
        step = .1
      ),

      # Slider for the number of rounds of the simulation
      sliderInput(
        "RoundsOfPlay",
        "Number of Rounds:",
        min = 10,
        max = 100,
        value = 20,
        step = 1
      ),

      # Action button to run simulation
      p(actionButton("runSimulation", "Run Simulation"), align = "center")

    ),

    # Animation of network formation via reinforcement learning
    mainPanel(
      ndtv:::ndtvAnimationWidgetOutput("netPlot", width = "100%", height = "640")
      )

  )

))
