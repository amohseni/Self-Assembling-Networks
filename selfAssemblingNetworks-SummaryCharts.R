### SELF-ASSEMBLING GAME (Signalling Chain)

# Install the igraph package
library(igraph)
library(gtools)
  
# Global variables
N <- 5 # population size
S <- 100 # number of runs of the whole simulation
t <- 100 # number of generations
r <- 100 # number of rounds of play with each generations 
perfectCommunication <- FALSE # whether agents communicate perfectly
edgeWeightCoefficient <- 10 # this coefficient controls the mean edge weight in the directed graph
reliablity.Nature.inf <- 0 # minimum reliability of agents when interacting with Nature
reliablity.Nature.sup <- 1 # maximum reliability of agents when interacting with Nature
reliablity.agents.inf <- .5 # minimum reliability of agents when interacting with other agents
reliablity.agents.sup <- 1 # maximum reliability of agents when interacting with other agents

### Repeat the whole simulation S times
for (k in 1:S) {
  
# Agent properties
agent0 <- c(TRUE) # create Nature, who always knows the truth
for (i in 1:10) { # create N agents
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

# Play game
for (i in 1:t) { # Set up the for-loop to go for t generations
  # Recall, the state of nature changes after each generation,
  # But is constant throughout the r rounds within each generation.

  for (i in 1:r) {  # Set up the for-loop to go for r rounds within each generation
    
    # Generate a random order for each generation of play
    orderOfPlay <- sample(1:N, N, replace=FALSE)
      # print(paste(">> Generation ", i, " order of play:", sep=""))
      # print(orderOfPlay)
      
    # Set up the for-loop for each agent's interation
    for (i in orderOfPlay) {
    
      # Select an interaction partner from the agent's urn
      interactionPartnerBall <- sample(eval(parse(text=paste("urn", i, sep=""))), 1, replace=TRUE) 
      interactionPartnerVector <- eval(parse(text=paste("agent", interactionPartnerBall, sep=""))) 
   
      # If the interactionPartner is Nature, 
      # then the agent succeeds in learning the true state of the world 
      # with probability corresponding to her reliability.
      if (interactionPartnerBall == 0) {
          # print(paste("Agent", i, " chooses to confer with Nature", sep=""))
        
        # Nature draws a random difficulty value in [0,1]
        NatureDifficulty <- round(runif(1, 0, 1), digits=2) 
          # print(paste("(Reliability of ", eval(parse(text=paste("reliability1", i, sep=""))), " vs difficulty of ", NatureDifficulty, ")", sep=""))
        
        # And by comparison with the agents reliability, we determine if the agent succeeds in learning the true state of the world,
        if (eval(parse(text=paste("reliability1", i, sep=""))) >= NatureDifficulty) { 
          # Update agent's knowledgeState to TRUE
          assign(paste("agent", i, sep=""), `[<-`(eval(parse(tex=paste("agent", i, sep=""))), 1, TRUE)) 
          # Reinforce the Nature ball in the agent's urn
          assign(paste("urn", i, sep=""), append(eval(parse(text=paste("urn", i, sep=""))), c(interactionPartnerBall), after = length(eval(parse(text=paste("urn", i, sep=""))))))
            # print(paste("agent", i, " succeeds in learning the true state of the world,", sep=""))
            # print("so, reinforce Nature ball.")
            # print(paste("Agent", i, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", i, sep=""))))
        } 
        # Or fails to learn the true state of the world.
        if (eval(parse(text=paste("reliability1", i, sep=""))) < NatureDifficulty) {       
          # Update agent's knowledgeState to FALSE,
          assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, FALSE)) 
            # print(paste("agent", i, " fails to learn the true state of the world.", sep=""))
            # print(paste("Agent", i, " urn is now:", sep=""))
            # print(eval(parse(text=paste("urn", i, sep=""))))
        }
      }
        
      # Alternatively, if the interactionPartner is another agent
      if (interactionPartnerBall %in% 1:N) {
      
        # (Print the agent selected, and their corresponding knowledgeState)
          # print(paste("Agent", i, " chooses to interact with agent", interactionPartnerBall, ",", sep="")) 
          # print(paste("and agent", interactionPartnerBall, " knowledgeState is ", interactionPartnerVector[1], ",", sep=""))  
        
        ### Where communication between agents is perfect, we proceed as follows ###
        if (perfectCommunication == TRUE) {  
          
          # Then, if the interactionPartner knows the correct state of the world, 
          # the agent learns the correct state of the world as well, 
          # and reinforces.
          if (interactionPartnerVector[1] == TRUE) { 
            # Update agenti's knowledgeState to TRUE
            assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, TRUE)) 
              # print(paste("so, reinforce agent", interactionPartnerBall, " ball.", sep ="")) 
            # Reinfoce the corresponding interactionPartner's ball in the agent's urn
            assign(paste("urn", i, sep=""), append(eval(parse(text=paste("urn", i, sep=""))), c(interactionPartnerBall), after = length(eval(parse(text=paste("urn", i, sep=""))))))
              # print(paste("Agent", i, " urn is now:", sep=""))
              # print(eval(parse(text=paste("urn", i, sep=""))))
          }
          # And, if the interactionPartner doesn't know the correct state of the world, 
          # then the agent infers a false state of the world 
          # and does not reinforce.
          if (interactionPartnerVector[1] == FALSE) {
            # Update agenti's knowledgeState to FALSE
            assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, FALSE)) 
              # print(paste("so, do NOT reinforce agent", interactionPartnerBall, " ball.", sep =""))
              # print(paste("Agent", i, " urn is now:", sep=""))
              # print(eval(parse(text=paste("urn", i, sep=""))))
          }
        } # End of PerfectCommuniation == TRUE case
        
        ### Alternatively, when communication between agents is *imperfect*, we proceed as follows ###
        if (perfectCommunication == FALSE) {
          
          # We draw a random difficulty value in [0,1]
          communicationDifficulty <- round(runif(1, 0, 1), digits=2) 
            # print(paste("(Reliability of ", eval(parse(text=paste("reliability2", i, sep=""))), " vs difficulty of ", communicationDifficulty, ")", sep=""))
          
          # And by comparison with the agents reliability, we determine if the agent succeeds in learning from the other agent's knowledge.
          # If communcation is successful…
          if (eval(parse(text=paste("reliability2", i, sep=""))) >= communicationDifficulty) {
            
            # Then, if the interactionPartner knows the correct state of the world, 
            # the agent learns the correct state of the world as well, 
            # and reinforces.
            if (interactionPartnerVector[1] == TRUE) { 
              # Update agenti's knowledgeState to TRUE
              assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, TRUE)) 
              # print(paste("so, reinforce agent", interactionPartnerBall, " ball.", sep ="")) 
              # Reinfoce the corresponding interactionPartner's ball in the agent's urn
              assign(paste("urn", i, sep=""), append(eval(parse(text=paste("urn", i, sep=""))), c(interactionPartnerBall), after = length(eval(parse(text=paste("urn", i, sep=""))))))
              # print(paste("Agent", i, " urn is now:", sep=""))
              # print(eval(parse(text=paste("urn", i, sep=""))))
            }
            # And, if the interactionPartner doesn't know the correct state of the world, 
            # then the agent infers a false state of the world 
            # and does not reinforce.
            if (interactionPartnerVector[1] == FALSE) {
              # Update agenti's knowledgeState to FALSE
              assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, FALSE)) 
              # print(paste("so, do NOT reinforce agent", interactionPartnerBall, " ball.", sep =""))
              # print(paste("Agent", i, " urn is now:", sep=""))
              # print(eval(parse(text=paste("urn", i, sep=""))))
            }
          }
          
          # Or if communication is unsuccessful,
          if (eval(parse(text=paste("reliability2", i, sep=""))) < communicationDifficulty) {       
            # Then update agent's knowledgeState to FALSE.
            assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, FALSE)) 
              # print(paste("agent", i, " fails to learn from their partner.", sep=""))
              # print(paste("Agent", i, " urn is now:", sep=""))
              # print(eval(parse(text=paste("urn", i, sep=""))))
          }
        }
      } # End of PerfectCommuniation == FALSE case
      
    } # End of round of play
  } # End of generation of play
  
  # Reset the agent's to a state of ignorance at the end of each generation
  for (i in 1:N) { # create N agents
    assign(paste("agent", i, sep=""), `[<-`(eval(parse(text=paste("agent", i, sep=""))), 1, FALSE)) 
  } 
} # End play of Game

##### From here on, we are done with the simulation itself, #####
##### and are aggregating & graphing the simulation results #####

  # Create a vector of the reliability1 values (w.r.t Nature) of agents in the population
  reliability1Vector <- c(1:N)
  for (i in 1:N) {
    reliability1Vector[i] <- c(eval(parse(text=paste("reliability1", i, sep=""))))
  }
    # (Append a NA value for Nature at the beginning of the reliabilityVector)
    reliability1Vector <- append(reliability1Vector, NA, after=0)

  # Create a vector of the reliability2 values (w.r.t the other agents) of agents in the population
  reliability2Vector <- c(1:N)
  for (i in 1:N) {
    reliability2Vector[i] <- c(eval(parse(text=paste("reliability2", i, sep=""))))
  }
    # (Append a NA value for Nature at the beginning of the reliabilityVector)
    reliability2Vector <- append(reliability2Vector, NA, after=0)    
    
  # (Create an empty vector for the final success rate of agents)
  successRate <- c()
  
  # (Create an empty vector of the proportion of each player's urn is Nature balls)
  reinfOnNatureVector <- c() 
  
  # (Create an empty matrix for the populationResultsTable (to be used later))
  populationResultsTable <- data.frame(matrix(0, ncol=3, nrow=(N+1)))
  colnames(populationResultsTable) <- c("Agent", "Count", "Frequency")
  populationResultsTable[1] <- c("n", 1:N) # we index the agents in the first column
  
  # Print the frequencies of reinforcement from each of the player urns  
  for (i in 1:N) {
      # print(paste("Agent", i, " urn distribution", sep=""))
    agentLabel <- c("n",1:N)
    urnTotals <- data.frame(agentLabel, tabulate(eval(parse(text=paste("urn", i, sep="")))+1, nbins=(N+1))) # Create a table with the ball totals of each interactionPartner
    urnTotalsDistribution <- round(urnTotals[2] / sum(urnTotals[2]), digits=2) # Create a table with the percentages for the ball totals
    resultsTable <- data.frame(urnTotals, urnTotalsDistribution, reliability1Vector, reliability2Vector) # Combine the two tables into resultsTable
    colnames(resultsTable) <- c("Agent", "Count", "Frequency", "Reliability (w/ Nature)", "Reliability (w/ Agents)") # Name the columns
      # print(resultsTable)
    # Compile the frequency of reinforcement on Nature
    reinfOnNatureVector <- append(reinfOnNatureVector, urnTotalsDistribution[1,1], after=length(reinfOnNatureVector))
    # Compile the success rate of reinforcements for each agent
    successRate <- append(successRate, sum(urnTotals[,2])/(t*r/N), after=length(successRate))
    # Complile the individual agent's reinforcement frequencies 
    # into a table of the whole population's reinforcement frequencies
    populationResultsTable[2:3] <- (populationResultsTable[2:3] + resultsTable[2:3]) # Add all the resultsTables together to get the populationResultsTable 
  }
  
  # Create the final populationResultsTable
  populationResultsTable[3] <- (round(populationResultsTable[3] / N, digits=2))
  populationResultsTable[,"Reliability (Nature)"] <- reliability1Vector # Add the reliability values to the right column
  populationResultsTable[,"Reliability (Agents)"] <- reliability2Vector # Add the reliability values to the right column
  populationResultsTable[,"Reinf on N"] <- c(NA, reinfOnNatureVector) # Add the reinforcment on nature values to the right hand column
  populationResultsTable[,"Success rate"] <- c(NA, successRate) # Add the success rate value to the right hand column
  
  # This store the results of multiple simulations in one data frame
  simulationResultsData <- rbind(simulationResultsData, populationResultsTable[1:N+1,])
  
} # END OF SIMULATION & DATA AGGREGATION

###################################################################
################ Begin GRAPHING the final data set ################
###################################################################

  # Output the final data set
  # simulationResultsData <- populationResultsTable[1,] # /RUN THIS BEFORE A NEW RUN OF SIMULATIONS/
  # simulationResultsData <- simulationResultsData[-1,] # /RUN THIS BEFORE A NEW RUN OF SIMULATIONS/
  print(simulationResultsData)
  
  # Create a linear regression model of agent EPISTEMIC RELIABILITY vs. frequency of BEING CONSULTED BY POPULATION
  rel <- c(simulationResultsData[2:nrow(simulationResultsData),4])
  reinf <- c(simulationResultsData[2:nrow(simulationResultsData),3])
  linearRegression <- lm(reinf~rel)
  # Create a scatterplot of agent reliability vs. frequency of reinforcement
  par(xpd=FALSE)
  plot(rel, reinf, 
       xlim = c(0,1), ylim = c(0,.5),
       type="p", xlab="Agent Epistemic Reliability", ylab="Frequency of Being Consulted by Population", main="Epistemic Reliability and Frequency of \nBeing Consulted by Population")
  abline(linearRegression) # Plot the linear regression model
  
  # Create a linear regression model of agent SOCIAL RELIABILITY vs. frequency of BEING CONSULTED BY POPULATION
  socRel <- c(simulationResultsData[2:nrow(simulationResultsData),5])
  reinf <- c(simulationResultsData[2:nrow(simulationResultsData),3])
  linearRegression <- lm(reinf~rel)
  # Create a scatterplot of agent reliability vs. frequency of reinforcement
  par(xpd=FALSE)
  plot(socRel, reinf, 
       xlim = c(.5,1), ylim = c(0,.5),
       type="p", xlab="Agent Social Reliability", ylab="Frequency of Being Consultd by Population", main="Social Reliability and Frequency of \nBeing Consulted by Population")
  abline(linearRegression) # Plot the linear regression model
  
  # Create a linear regression model of agent EPISTEMIC RELIABILITY vs. frequency of REINFORCEMENT on NATURE
  rel <- c(simulationResultsData[2:nrow(simulationResultsData),4])
  reinfN <- c(simulationResultsData[2:nrow(simulationResultsData),6])
  linearRegression <- lm(reinf~rel)
  # Create a scatterplot of agent reliability vs. frequency of reinforcement
  par(xpd=FALSE)
  plot(rel, reinfN, 
       xlim = c(0,1), ylim = c(0,1),
       type="p", xlab="Agent Epistemic Reliability", ylab="Frequency of Conferring with Nature", main="Epistemic Reliability \nand Conferring with Nature")
  abline(linearRegression) # Plot the linear regression model
  
  # Create a linear regression model of agent SOCIAL RELIABILITY vs. frequency of REINFORCEMENT on NATURE
  socRel <- c(simulationResultsData[2:nrow(simulationResultsData),5])
  reinfN <- c(simulationResultsData[2:nrow(simulationResultsData),6])
  linearRegression <- lm(reinf~rel)
  # Create a scatterplot of social agent reliability vs. frequency of reinforcement on nature
  par(xpd=FALSE)
  plot(socRel, reinfN, 
       xlim = c(.5,1), ylim = c(0,1),
       type="p", xlab="Agent Social Reliability", ylab="Frequency of Conferring with Nature", main="Social Reliability \nand Conferring with Nature")
  abline(linearRegression) # Plot the linear regression model
  
  # Print the simulation's parameters
  print(paste("Simulation parameters: Population size = ", N, 
              ", Generations = " , t, ", Rounds = ", r, 
              ", perfect communication = ", perfectCommunication,
              sep=""))
  
  ###################################################
  # General popualtion summar statistics
  ###################################################
  summary(simulationResultsData)
  
  # simulationResultsData.PerfectCommunication.N5.S100.t100.r100 <- simulationResultsData
  # write.csv(mydf, file = "saveddf.csv")
  
  # Select & Summarize top 1aurtile w.r.t Reliability
  simulationResults.Reliability.Top <- subset(simulationResultsData, simulationResultsData[4] > .75)
  summary(simulationResults.Reliability.Top)
  # Select & Summarize bottom 1aurtile w.r.t Reliability
  simulationResults.Reliability.Bottom <- subset(simulationResultsData, simulationResultsData[4] <= .25)
  summary(simulationResults.Reliability.Bottom)
  
  # Select & Summarize top 1aurtile w.r.t Reliability
  simulationResults.SocialReliability.Top <- subset(simulationResultsData, simulationResultsData[5] >= .875)
  summary(simulationResults.SocialReliability.Top)
  # Select & Summarize bottom 1aurtile w.r.t Reliability
  simulationResults.SocialReliability.Bottom <- subset(simulationResultsData, simulationResultsData[5] <= .625)
  summary(simulationResults.SocialReliability.Bottom)
  
  ###################################################
  # SAVED DATA
  ###################################################
  
  # setwd("~/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Self-Assembling Games/RL | R")
  # simulationResultsData.PerfectCommunication.N5.S100.t100.r100 <- simulationResultsData
  # write.csv(simulationResultsData, file = "simulationResultsData.PerfectCommunication.N5.S100.t100.r100.csv")
  # write.csv(simulationResultsData, file = "simulationResultsData.ImperfectCommunication.N5.S100.t100.r100.csv")
  # write.csv(simulationResultsData, file = "simulationResultsData.PerfectCommunication.N20.S100.t100.r100.csv")
  # write.csv(simulationResultsData, file = "simulationResultsData.ImperfectCommunication.N20.S100.t100.r100.csv")
  
  