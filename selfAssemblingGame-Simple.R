###################################################
# SELF-ASSEMBLING GAME (Signalling Chain) v9.7
###################################################
# Created by: Aydin Mohseni 
# Contact: aydin.mohseni@gmail.com
# URL: www.aydinmohseni.com

# Install relevant packages from the library
library(reshape2)
library(ndtv)
library(tsna)  

# Global variables
N <- 8 # population size
roundsOfPlay <- 100 # number of rounds of play with each generations 
perfectCommunication <- TRUE # whether agents communicate perfectly
reliablity.Nature.inf <- 0 # minimum reliability of agents when interacting with Nature
reliablity.Nature.sup <- 1 # maximum reliability of agents when interacting with Nature
reliablity.agents.inf <- 0 # minimum reliability of agents when interacting with other agents
reliablity.agents.sup <- 1 # maximum reliability of agents when interacting with other agents
 
# Create a data frame in which to save the simulation results
# specifically, the edge-lists of the network for each round of play
adjMatrixOverTime <- list()
initialAdjMatrix <- matrix(data = 0, nrow = N + 1, ncol = N + 1)
rownames(initialAdjMatrix) <- c("n", 1:N) # label the rows
colnames(initialAdjMatrix) <- c("n", 1:N) # label the columns
adjMatrixOverTime[[1]] <- initialAdjMatrix

###################################################  
## Agent properties
###################################################  
agent0 <- c(TRUE) # create Nature, who always knows the truth
for (i in 1:N) { # create N agents
  # Set up agent urn with one ball for Nature (0), and one ball for each agent (1-N)
  assign(paste("urn", i, sep=""), c(0:N)) 
  # generate a reliability value in [0,1] -- for inferring the correct state of Nature
  assign(paste("reliability1", i, sep=""), round(runif(1, reliablity.Nature.inf, reliablity.Nature.sup), digits=3)) 
  # generate a reliability value in [0,1] -- for learning the correct state from other agents
  assign(paste("reliability2", i, sep=""), round(runif(1, reliablity.agents.inf, reliablity.agents.sup), digits=3)) 
  # agent begins in a state of ignorance
  assign(paste("knowledgeState", i, sep=""), FALSE) 
  # agent is assigned an urn, reliability, and knowledgeState
  assign(paste("agent", i, sep=""), 
         c(eval(parse(text=paste("knowledgeState", i, sep=""))), 
           eval(parse(text=paste("reliability1", i, sep=""))), 
           eval(parse(text=paste("reliability2", i, sep=""))),
           eval(parse(text=paste("urn", i, sep=""))))) 
}

###################################################  
## Play game
###################################################
for (round in 1:roundsOfPlay) {  # Set up the for-loop to go for rounds of play 
  
  # Generate a random order for each generation of play
  orderOfPlay <- sample(1:N, N, replace=FALSE)
    # print(paste(">> Generation ", generation, " order of play:", sep=""))
    # print(orderOfPlay)
    
  # Set up the for-loop for each agent's interation
  for (randomAgent in 1:N) {
    
    # Select a random agent to attempt to play: 
    # that is, to attempt to learn the true state of Nature
    # either by investigating Nature directly, 
    # or consulting one of her peers
    focalAgent <- orderOfPlay[randomAgent]
    
    # Select an interaction partner from the agent's urn
    interactionPartnerBall <- sample(eval(parse(text=paste("urn", focalAgent, sep=""))), 1, replace=TRUE) 
    interactionPartnerVector <- eval(parse(text=paste("agent", interactionPartnerBall, sep=""))) 
 
    ###################################################
    # If the chosen interaction partner is Nature, 
    ###################################################
    # then the agent succeeds in learning the true state of the world 
    # with probability corresponding to her reliability.
    if (interactionPartnerBall == 0) {
        # print(paste("Agent", focalAgent, " chooses to confer with Nature", sep=""))
      
      # Nature draws a random difficulty value in [0,1]
      NatureDifficulty <- round(runif(1, 0, 1), digits = 3) 
        # print(paste("(Reliability of ", eval(parse(text=paste("reliability1", focalAgent, sep=""))), " vs difficulty of ", NatureDifficulty, ")", sep=""))
      
      # And by comparison with the agents reliability, we determine if the agent succeeds in learning the true state of the world,
      if (eval(parse(text=paste("reliability1", focalAgent, sep=""))) >= NatureDifficulty) { 
        # Update agent's knowledgeState to TRUE
        assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(tex=paste("agent", focalAgent, sep=""))), 1, TRUE)) 
        # Reinforce the Nature ball in the agent's urn
        assign(paste("urn", focalAgent, sep=""), append(eval(parse(text=paste("urn", focalAgent, sep=""))), c(interactionPartnerBall), after = length(eval(parse(text=paste("urn", focalAgent, sep=""))))))
          # print(paste("agent", focalAgent, " succeeds in learning the true state of the world,", sep=""))
          # print("so, reinforce Nature ball.")
          # print(paste("Agent", focalAgent, " urn is now:", sep=""))
          # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
      } 
      # Or fails to learn the true state of the world.
      if (eval(parse(text=paste("reliability1", focalAgent, sep=""))) < NatureDifficulty) {       
        # Update agent's knowledgeState to FALSE,
        assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(text=paste("agent", focalAgent, sep=""))), 1, FALSE)) 
          # print(paste("agent", focalAgent, " fails to learn the true state of the world.", sep=""))
          # print(paste("Agent", focalAgent, " urn is now:", sep=""))
          # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
      }
    }
    
    ###################################################  
    ## Alternatively, if the chosen interaction partner is another agent
    ###################################################
    if (interactionPartnerBall %in% 1:N) {
    
      # (Print the agent selected, and their corresponding knowledgeState)
        # print(paste("Agent", focalAgent, " chooses to interact with agent", interactionPartnerBall, ",", sep="")) 
        # print(paste("and agent", interactionPartnerBall, " knowledgeState is ", interactionPartnerVector[1], ",", sep=""))  
      
      ###################################################
      ## Where communication between agents is perfect, we proceed as follows
      ###################################################
      if (perfectCommunication == TRUE) {  
        
        # Then, if the interactionPartner knows the correct state of the world, 
        # the agent learns the correct state of the world as well, 
        # and reinforces.
        if (interactionPartnerVector[1] == TRUE) { 
          # Update agenti's knowledgeState to TRUE
          assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(text=paste("agent", focalAgent, sep=""))), 1, TRUE)) 
            # print(paste("so, reinforce agent", interactionPartnerBall, " ball.", sep ="")) 
          # Reinfoce the corresponding interactionPartner's ball in the agent's urn
          assign(paste("urn", focalAgent, sep=""), append(eval(parse(text=paste("urn", focalAgent, sep=""))), c(interactionPartnerBall), after = length(eval(parse(text=paste("urn", focalAgent, sep=""))))))
            # print(paste("Agent", focalAgent, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
        }
        # And, if the interactionPartner doesn'numberOfGenerations know the correct state of the world, 
        # then the agent infers a false state of the world 
        # and does not reinforce.
        if (interactionPartnerVector[1] == FALSE) {
          # Update agenti's knowledgeState to FALSE
          assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(text=paste("agent", focalAgent, sep=""))), 1, FALSE)) 
            # print(paste("so, do NOT reinforce agent", interactionPartnerBall, " ball.", sep =""))
            # print(paste("Agent", focalAgent, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
        }
      } # End of PerfectCommuniation == TRUE case
      
      ###################################################
      ## Alternatively, when communication between agents is *imperfect*, we proceed as follows
      ###################################################
      if (perfectCommunication == FALSE) {
        
        # We draw a random difficulty value in [0,1]
        communicationDifficulty <- round(runif(1, 0, 1), digits=3) 
          # print(paste("(Reliability of ", eval(parse(text=paste("reliability2", focalAgent, sep=""))), " vs difficulty of ", communicationDifficulty, ")", sep=""))
        
        # And by comparison with the agents reliability, we determine if the agent succeeds in learning from the other agent's knowledge.
        # If communcation is successful…
        if (eval(parse(text=paste("reliability2", focalAgent, sep=""))) >= communicationDifficulty) {
          
          # Then, if the interactionPartner knows the correct state of the world, 
          # the agent learns the correct state of the world as well, 
          # and reinforces.
          if (interactionPartnerVector[1] == TRUE) { 
            # Update agenti's knowledgeState to TRUE
            assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(text=paste("agent", focalAgent, sep=""))), 1, TRUE)) 
            # print(paste("so, reinforce agent", interactionPartnerBall, " ball.", sep ="")) 
            # Reinfoce the corresponding interactionPartner's ball in the agent's urn
            assign(paste("urn", focalAgent, sep=""), append(eval(parse(text=paste("urn", focalAgent, sep=""))), c(interactionPartnerBall), after = length(eval(parse(text=paste("urn", focalAgent, sep=""))))))
            # print(paste("Agent", focalAgent, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
          }
          # And, if the interactionPartner doesn'numberOfGenerations know the correct state of the world, 
          # then the agent infers a false state of the world 
          # and does not reinforce.
          if (interactionPartnerVector[1] == FALSE) {
            # Update agenti's knowledgeState to FALSE
            assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(text=paste("agent", focalAgent, sep=""))), 1, FALSE)) 
            # print(paste("so, do NOT reinforce agent", interactionPartnerBall, " ball.", sep =""))
            # print(paste("Agent", focalAgent, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
          }
        }
        
        # Or if communication is unsuccessful,
        if (eval(parse(text=paste("reliability2", focalAgent, sep=""))) < communicationDifficulty) {       
          # Then update agent's knowledgeState to FALSE.
          assign(paste("agent", focalAgent, sep=""), `[<-`(eval(parse(text=paste("agent", focalAgent, sep=""))), 1, FALSE)) 
            # print(paste("agent", focalAgent, " fails to learn from their partner.", sep=""))
            # print(paste("Agent", focalAgent, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", focalAgent, sep=""))))
        }
      }
    } # End of PerfectCommuniation == FALSE case
    
  } # End of round of play
  
  # Create a new adjacency matrix recording the new network structure
  adjMatrix <- data.frame(matrix(data = 0, nrow = N + 1, ncol = N + 1))
  rownames(adjMatrix) <- c("n", 1:N) # label the rows
  colnames(adjMatrix) <- c("n", 1:N) # label the columns
  for (agent in 1:N) { 
    # For each agent, count up the balls in her urn for nature, 
    # and for each other agent
    urnCount <- data.frame(agent, tabulate(eval(parse(
      text = paste("urn", agent, sep = "")
    )) + 1, nbins = (N + 1)))
    # Count the total number of balls in her earn
    urnTotal <- length(eval(parse(text = paste("urn", agent, sep = ""))))
    for (j in 1:(N + 1)) {
      # For each ball type in the focal agent's urn, 
      # determine if the current number of balls 
      # exceeds the proportion 1/(N+1) of her total number of balls.
      # If so, in the adjacency matrix, count the focal agent 
      # as having developed a 'link' to that agent
      if (urnCount[j,2] / urnTotal > (N + 1) ^ -1) {
        adjMatrix[agent, j] <- 1
      } else {
        # If not, in the adjacency matrix, count the focal agent
        # as having no 'link' to that agent
        adjMatrix[agent, j] <- 0
      }
    }
  }
  # Save the adjacency matrix for this round
  # to the time-indexed list of adjacency matrices
  adjMatrixOverTime[[round + 1]] <- adjMatrix
  # adjMatrixOverTime[[((round - 1) * N + randomAgent)]] <- as.network(adjMatrix)
  
} # End of simulation
  
dfAdjacencyMatrixList <- lapply(adjMatrixOverTime, as.network.matrix, matrix.type = 'adjacency')
dfDynamic <- networkDynamic(network.list = dfAdjacencyMatrixList)
animationSlice <- list(
  start = 0,
  end = roundsOfPlay,
  interval = 1,
  aggregate.dur = 0,
  rule = "latest"
)
compute.animation(
  dfDynamic,
  slice.par = animationSlice,
  animation.mode = 'MDSJ',
  verbose = FALSE
)
render.d3movie(dfDynamic,
               render.par = list(
                 tween.frames = 1,
                 show.time = FALSE,
                 show.stats = NULL,
                 extraPlotCmds = NULL               
                 ),
               plot.par = list(
                 bg = 'white',
                 vertex.cex = 1.5,
                 vertex.col = c("darkgreen", rep("black", N)),
                 vertex.border = c("darkgreen", rep("black", N)),
                 edge.col = "darkgray",
                 label.cex = 1,
                 displaylabels = TRUE
               ), 
               d3.options = list(
               animateOnLoad = TRUE,
               animationDuration = 400,
               durationControl = FALSE,
               slider = TRUE
               ),
               animation.mode = 'MDSJ', 
               script.type = 'embedded', 
               output.mode = 'htmlWidget'
               )
# render.animation(
#   dfDynamic,
#   render.par = list(tween.frames = 10),
#   vertex.col = 'blue',
#   edge.col = 'gray'
# )



## EOD


 