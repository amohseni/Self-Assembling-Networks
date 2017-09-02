# SELF-ASSEMBLING NETWORKS
# << SERVER >>
# by Aydin Mohseni


### Install packages
library(shiny)
library(reshape2)
library(ndtv)
library(network)
library(htmlwidgets)


### Define server logic for Application
shinyServer(function(input, output, session) {
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    # Take a depdendency with the runSimulation action button,
    # causing the simulation to be run when it is pressed
    input$runSimulation

    withProgress(message = 'Making plot:', value = 0, {
      # Global variables
      N <- isolate(input$PopulationSize) # population size
      roundsOfPlay <-
        isolate(input$RoundsOfPlay) # number of rounds of play with each generations
      perfectCommunication <-
        isolate((input$PerfectCommunication == "Perfect")) # whether agents communicate perfectly
      reliablity.Nature.inf <-
        0 # minimum reliability of agents when interacting with Nature
      reliablity.Nature.sup <-
        1 # maximum reliability of agents when interacting with Nature
      reliablity.agents.inf <-
        .5 # minimum reliability of agents when interacting with other agents
      reliablity.agents.sup <-
        1 # maximum reliability of agents when interacting with other agents
      generation <- 10 # rounds for which the state of the world is the same
      threshold <- isolate(input$EdgeThreshold)

      # Create vectors in which to save agent reliability values
      reliablity.epistemic.vector <- rep(NA, N + 1)
      reliablity.social.vector <- rep(NA, N + 1)

      # Create a data frame in which to save the simulation results
      # specifically, the edge-lists of the network for each round of play
      adjMatrixOverTime <- list()
      initialAdjMatrix <- matrix(data = 0,
                                 nrow = N + 1,
                                 ncol = N + 1)
      rownames(initialAdjMatrix) <- c("n", 1:N) # label the rows
      colnames(initialAdjMatrix) <- c("n", 1:N) # label the columns
      adjMatrixOverTime[[1]] <- initialAdjMatrix

      # --------------------------------------------------
      # Agent properties
      # --------------------------------------------------
      agent0 <- c(TRUE) # create Nature, who always knows the truth
      for (i in 1:N) {
        # create N agents
        # Set up agent urn with one ball for Nature (0), and one ball for each agent (1-N)
        assign(paste("urn", i, sep = ""), c(0:N))
        # generate and record an epistemic reliability value in [0,1] -- for inferring the correct state of Nature
        reliablity.epistemic.vector[i + 1] <- assign(paste("reliability1", i, sep = ""),
               round(
                 runif(1, reliablity.Nature.inf, reliablity.Nature.sup),
                 digits = 3
               ))
        # generate and record a social reliability value in [0,1] -- for learning the correct state from other agents
        reliablity.social.vector[i + 1] <- assign(paste("reliability2", i, sep = ""),
               round(
                 runif(1, reliablity.agents.inf, reliablity.agents.sup),
                 digits = 3
               ))
        # agent begins in a state of ignorance
        assign(paste("knowledgeState", i, sep = ""), FALSE)
        # agent is assigned an urn, reliability, and knowledgeState
        assign(paste("agent", i, sep = ""),
               c(
                 eval(parse(
                   text = paste("knowledgeState", i, sep = "")
                 )),
                 eval(parse(text = paste(
                   "reliability1", i, sep = ""
                 ))),
                 eval(parse(text = paste(
                   "reliability2", i, sep = ""
                 ))),
                 eval(parse(text = paste(
                   "urn", i, sep = ""
                 )))
               ))
      }

      # --------------------------------------------------
      # Play game
      # --------------------------------------------------
      for (round in 1:roundsOfPlay) {
        # Set up the for-loop to go for rounds of play

        # Every generation, Nature changes,
        # and we reset the agents' to a state of ignorance
        if (round %% generation == 0) {
          for (i in 1:N) {
            assign(paste("agent", i, sep = ""), `[<-`(eval(parse(
              text = paste("agent", i, sep = "")
            )), 1, FALSE))
          }
        }

        # Generate a random order for each round of play
        orderOfPlay <- sample(1:N, N, replace = FALSE)

        # Set up the for-loop for each agent's interation
        for (randomAgent in 1:N) {
          # Select a random agent to attempt to play:
          # that is, to attempt to learn the true state of Nature
          # either by investigating Nature directly,
          # or consulting one of her peers
          focalAgent <- orderOfPlay[randomAgent]

          # Select an interaction partner from the agent's urn
          interactionPartnerBall <-
            sample(eval(parse(
              text = paste("urn", focalAgent, sep = "")
            )), 1, replace = TRUE)
          interactionPartnerVector <-
            eval(parse(text = paste(
              "agent", interactionPartnerBall, sep = ""
            )))

          # --------------------------------------------------
          # If the chosen interaction partner is Nature,
          # --------------------------------------------------
          # then the agent succeeds in learning the true state of the world
          # with probability corresponding to her reliability.
          if (interactionPartnerBall == 0) {

            # Nature draws a random difficulty value in [0,1]
            NatureDifficulty <- round(runif(1, 0, 1), digits = 3)

            # And by comparison with the agents reliability,
            # we determine if the agent succeeds in learning the true state of the world,
            if (eval(parse(text = paste(
              "reliability1", focalAgent, sep = ""
            ))) >= NatureDifficulty) {
              # Update agent's knowledgeState to TRUE
              assign(paste("agent", focalAgent, sep = ""),
                     `[<-`(eval(parse(
                       tex = paste("agent", focalAgent, sep = "")
                     )), 1, TRUE))
              # Reinforce the Nature ball in the agent's urn
              assign(paste("urn", focalAgent, sep = ""),
                     append(
                       eval(parse(
                         text = paste("urn", focalAgent, sep = "")
                       )),
                       c(interactionPartnerBall),
                       after = length(eval(parse(
                         text = paste("urn", focalAgent, sep = "")
                       )))
                     ))
            }
            # Or fails to learn the true state of the world.
            if (eval(parse(text = paste(
              "reliability1", focalAgent, sep = ""
            ))) < NatureDifficulty) {
              # Update agent's knowledgeState to FALSE,
              assign(paste("agent", focalAgent, sep = ""),
                     `[<-`(eval(parse(
                       text = paste("agent", focalAgent, sep = "")
                     )), 1, FALSE))
            }
          }

          # --------------------------------------------------
          # Alternatively, if the chosen interaction partner is another agent
          # --------------------------------------------------
          if (interactionPartnerBall %in% 1:N) {

            # --------------------------------------------------
            # Where communication between agents is *perfect*, we proceed as follows
            # --------------------------------------------------
            if (perfectCommunication == TRUE) {
              # Then, if the interactionPartner knows the correct state of the world,
              # the agent learns the correct state of the world as well,
              # and reinforces.
              if (interactionPartnerVector[1] == TRUE) {
                # Update agenti's knowledgeState to TRUE
                assign(paste("agent", focalAgent, sep = ""),
                       `[<-`(eval(parse(
                         text = paste("agent", focalAgent, sep = "")
                       )), 1, TRUE))
                # Reinfoce the corresponding interactionPartner's ball in the agent's urn
                assign(paste("urn", focalAgent, sep = ""),
                       append(
                         eval(parse(
                           text = paste("urn", focalAgent, sep = "")
                         )),
                         c(interactionPartnerBall),
                         after = length(eval(parse(
                           text = paste("urn", focalAgent, sep = "")
                         )))
                       ))
              }
              # And, if the interactionPartner doesn't know the correct state of the world,
              # then the agent infers a false state of the world
              # and does not reinforce.
              if (interactionPartnerVector[1] == FALSE) {
                # Update agenti's knowledgeState to FALSE
                assign(paste("agent", focalAgent, sep = ""),
                       `[<-`(eval(parse(
                         text = paste("agent", focalAgent, sep = "")
                       )), 1, FALSE))
              }
            } # End of PerfectCommuniation == TRUE case

            # --------------------------------------------------
            ## Alternatively, when communication between agents is *imperfect*,
            # we proceed as follows
            # --------------------------------------------------
            if (perfectCommunication == FALSE) {
              # We draw a random difficulty value in [0,1]
              communicationDifficulty <-
                round(runif(1, 0, 1), digits = 3)

              # And by comparison with the agents reliability,
              # we determine if the agent succeeds in learning from the other agent's knowledge.
              # If communcation is successful…
              if (eval(parse(text = paste(
                "reliability2", focalAgent, sep = ""
              ))) >= communicationDifficulty) {
                # Then, if the interactionPartner knows the correct state of the world,
                # the agent learns the correct state of the world as well,
                # and reinforces.
                if (interactionPartnerVector[1] == TRUE) {
                  # Update agenti's knowledgeState to TRUE
                  assign(paste("agent", focalAgent, sep = ""),
                         `[<-`(eval(parse(
                           text = paste("agent", focalAgent, sep = "")
                         )), 1, TRUE))
                  # Reinfoce the corresponding interactionPartner's ball in the agent's urn
                  assign(
                    paste("urn", focalAgent, sep = ""),
                    append(
                      eval(parse(
                        text = paste("urn", focalAgent, sep = "")
                      )),
                      c(interactionPartnerBall),
                      after = length(eval(parse(
                        text = paste("urn", focalAgent, sep = "")
                      )))
                    )
                  )
                }
                # And, if the interactionPartner doesn't know the correct state of the world,
                # then the agent infers a false state of the world
                # and does not reinforce.
                if (interactionPartnerVector[1] == FALSE) {
                  # Update agenti's knowledgeState to FALSE
                  assign(paste("agent", focalAgent, sep = ""),
                         `[<-`(eval(parse(
                           text = paste("agent", focalAgent, sep = "")
                         )), 1, FALSE))
                }
              }

              # Or if communication is unsuccessful,
              if (eval(parse(text = paste(
                "reliability2", focalAgent, sep = ""
              ))) < communicationDifficulty) {
                # Then update agent's knowledgeState to FALSE.
                assign(paste("agent", focalAgent, sep = ""),
                       `[<-`(eval(parse(
                         text = paste("agent", focalAgent, sep = "")
                       )), 1, FALSE))
              }
            }
          } # End of PerfectCommuniation == FALSE case

          # --------------------------------------------------
        } # End of round of play
        # --------------------------------------------------

        # Create a new adjacency matrix recording the new network structure
        adjMatrix <-
          data.frame(matrix(
            data = 0,
            nrow = N + 1,
            ncol = N + 1
          ))
        rownames(adjMatrix) <- c("n", 1:N) # label the rows
        colnames(adjMatrix) <- c("n", 1:N) # label the columns
        for (agent in 1:N) {
          # For each agent, count up the balls in her urn for nature,
          # and for each other agent
          urnCount <- data.frame(agent, tabulate(eval(parse(
            text = paste("urn", agent, sep = "")
          )) + 1, nbins = (N + 1)))
          # Count the total number of balls in her earn
          urnTotal <-
            length(eval(parse(text = paste(
              "urn", agent, sep = ""
            ))))
          for (j in 1:(N + 1)) {
            # For each ball type in the focal agent's urn,
            # determine if the current number of balls
            # exceeds the proportion 1/(N+1) of her total number of balls.
            if (urnCount[j, 2] / urnTotal > (threshold / (N + 1))) {
              # If so, in the adjacency matrix, count the focal agent
              # as having developed a 'link' to that agent
              adjMatrix[agent + 1, j] <- 1
            } else {
              # If not, in the adjacency matrix, count the focal agent
              # as having no 'link' to that agent
              adjMatrix[agent + 1, j] <- 0
            }
          }
        }
        # Save the adjacency matrix for this round
        # to the time-indexed list of adjacency matrices
        adjMatrixOverTime[[round + 1]] <- adjMatrix

        # Increment the progress bar, and update the detail text.
        incProgress(1 / roundsOfPlay, detail = paste("rendering network", round, sep = " "))

      # --------------------------------------------------
      } # End of simulation
      # --------------------------------------------------

      # --------------------------------------------------
      # Create dynamic network animation
      # --------------------------------------------------

      # Convert adjacenty matrices to network matrices
      dfAdjacencyMatrixList <-
        lapply(adjMatrixOverTime, as.network.matrix, matrix.type = 'adjacency')
      # Convert network matrices to a network dynamic
      dfDynamic <-
        networkDynamic(network.list = dfAdjacencyMatrixList)
      # Select the subset of the dynamic to be animated:
      # specifically, remove the last (empty) network from the animation
      animationSlice <- list(
        start = 0,
        end = roundsOfPlay,
        interval = 1,
        aggregate.dur = 0,
        rule = "latest"
      )
      # Compute the animation of the dynamic
      compute.animation(
        dfDynamic,
        slice.par = animationSlice,
        animation.mode = 'MDSJ',
        verbose = FALSE
      )
      # Creat the vertex tooltip labels
      vertexToolTip <- c()
      reliablity.epistemic.vector <- round(reliablity.epistemic.vector, digits = 2)
      reliablity.social.vector <- round(reliablity.social.vector, digits = 2)
      vertexToolTip[1] <- paste('<strong>', 'Nature', '</strong>',
                                '<br>',
                                'Epistemic Reliability:',
                                '-',
                                '<br>',
                                'Social Reliability:',
                                '-',
                                sep = " ")
      for (i in 1:N) {
        vertexToolTip[i + 1] <- paste('<strong>', 'Agent', i, '</strong>',
                                      '<br>',
                                      'Epistemic Reliability:',
                                      reliablity.epistemic.vector[i + 1],
                                      '<br>',
                                      'Social Reliability:',
                                      reliablity.social.vector[i + 1],
                                      sep = " ")
      }
      # Output the network animation as an HTML widget
      render.d3movie(
        dfDynamic,
        # Interface functionality parameters
        render.par = list(
          tween.frames = 1,
          show.time = FALSE,
          show.stats = NULL,
          extraPlotCmds = NULL
        ),
        # Network visual parameters
        plot.par = list(
          bg = 'white',
          vertex.cex = 1.5,
          vertex.col = c("#4595F0", rep("gray20", N)),
          vertex.border = c("#4595F0", rep("gray20", N)),
          vertex.tooltip = vertexToolTip,
          edge.col = "darkgray",
          label.col = c("#4595F0", rep("gray20", N)),
          label.cex = 1,
          displaylabels = TRUE
        ),
        # Animation functionality parameters
        d3.options = list(
          animateOnLoad = TRUE,
          animationDuration = 600,
          durationControl = FALSE,
          slider = TRUE
        ),
        animation.mode = 'MDSJ',
        chain.direction = 'reverse',
        script.type = 'embedded',
        output.mode = 'htmlWidget'
      )
    })
  })

  # Update the maximum edge threshold numerator
  # to be: Population size + 1
  observe({
    N <- as.numeric(input$PopulationSize)
  # Control the max of the EdgeThreshold slider.
  updateSliderInput(session,
                    "EdgeThreshold",
                    max = N)

  })

})
### EOD ###
