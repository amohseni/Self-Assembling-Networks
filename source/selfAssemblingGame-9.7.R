###################################################
# SELF-ASSEMBLING GAME (Signalling Chain) v9.7
###################################################
# Created by: Aydin Mohseni 
# Contact: aydin.mohseni@gmail.com
# URL: www.aydinmohseni.com

# Install relevant packages from the library
library(gtools)
library(ggplot2)
library(reshape2)
  
# Global variables
N <- 20 # population size
t <- 200 # number of generations
r <- 100 # number of rounds of play with each generations 
S <- 100 # number of runs of the whole simulation
successRateInterval <- 1 # interval of number of generations in which we measure success rate of agents
successRateGraph <- c() # create empty vector for success rates
successRateGraphTOP <- c() # create empty vector for success rates
successRateGraphBOT <- c() # create empty vector for success rates
perfectCommunication <- FALSE # whether agents communicate perfectly
reliablity.Nature.inf <- 0 # minimum reliability of agents when interacting with Nature
reliablity.Nature.sup <- 1 # maximum reliability of agents when interacting with Nature
reliablity.agents.inf <- .5 # minimum reliability of agents when interacting with other agents
reliablity.agents.sup <- 1 # maximum reliability of agents when interacting with other agents
 
# Create a data frame in which to save the simulation results.
# Specifically, we will save each of the 'S' runs of the simulation (as rows of the data frame)
# with one containing the core statistical data (e.g., reliability values, urns compositions, and so on)
simulationResultsData <- data.frame(matrix(ncol=7,nrow=0))
colnames(simulationResultsData) <- c("Agent","Count","Frequency","Reliability (Nature)","Reliability (Agents)","Reinf on N","Success rate")
# And another containing the success rates for each of the 't' generations (corresponding to the columns of the data frame).
df <- data.frame(matrix(NA, nrow = S, ncol = t)) # This will contain the average population succes rates overtime
df.top <- data.frame(matrix(NA, nrow = S, ncol = t)) # The top quartile's success rates over time
df.bottom <- data.frame(matrix(NA, nrow = S, ncol = t)) # The bottom quartiles success rates over time

###################################################
## Repeat the whole simulation S times
###################################################
for (q in 1:S) {

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
for (i in 1:t) { # Set up the for-loop to play the game for t generations.
  # Recall: the state of nature changes after each generation,
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
   
      ###################################################
      # If the interactionPartner is Nature, 
      ###################################################
      # then the agent succeeds in learning the true state of the world 
      # with probability corresponding to her reliability.
      if (interactionPartnerBall == 0) {
          # print(paste("Agent", i, " chooses to confer with Nature", sep=""))
        
        # Nature draws a random difficulty value in [0,1]
        NatureDifficulty <- round(runif(1, 0, 1), digits=3) 
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
      
      ###################################################  
      ## Alternatively, if the interactionPartner is another agent
      ###################################################
      if (interactionPartnerBall %in% 1:N) {
      
        # (Print the agent selected, and their corresponding knowledgeState)
          # print(paste("Agent", i, " chooses to interact with agent", interactionPartnerBall, ",", sep="")) 
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
        
        ###################################################
        ## Alternatively, when communication between agents is *imperfect*, we proceed as follows
        ###################################################
        if (perfectCommunication == FALSE) {
          
          # We draw a random difficulty value in [0,1]
          communicationDifficulty <- round(runif(1, 0, 1), digits=3) 
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
  
  ###################################################
  ## Data aggregation between generations of play
  ################################################### 
  
  # Calculate the success rates of agents for this generation
  for (i in 1:N) {
    # Index each of the agents in the population
    agentLabel <- c("n",1:N)
    # Create a table with the ball totals of each interactionPartner
    urnTotals <- data.frame(agentLabel, tabulate(eval(parse(text=paste("urn", i, sep="")))+1, nbins=(N+1))) 
    # Compile the success rate of reinforcements for each agent
    successRateGraph <- append(successRateGraph, sum(urnTotals[,2]), after=length(successRateGraph))
  }
  
  # Calculate the success rates of the /top qaurtile/ of agents for this generation
  for (i in 1:N) {
    if (eval(parse(text=paste("reliability1", i, sep=""))) > 0.75) { # Include only agents with reliability > .75
    # Create a table with the ball totals of each interactionPartner
    urnTotalsTOP <- data.frame(agentLabel, tabulate(eval(parse(text=paste("urn", i, sep="")))+1, nbins=(N+1))) 
    # Compile the success rate of reinforcements for each agent
    successRateGraphTOP <- append(successRateGraphTOP, sum(urnTotalsTOP[,2]), after=length(successRateGraphTOP))
    }
  }
  
  # Calculate the success rates of the /bottom qaurtile/ of agents for this generation
  for (i in 1:N) {
    if (eval(parse(text=paste("reliability1", i, sep=""))) < 0.25) { # Include only agents with reliability < .25
      # Create a table with the ball totals of each interactionPartner
      urnTotalsBOT <- data.frame(agentLabel, tabulate(eval(parse(text=paste("urn", i, sep="")))+1, nbins=(N+1))) 
      # Compile the success rate of reinforcements for each agent
      successRateGraphBOT <- append(successRateGraphBOT, sum(urnTotalsBOT[,2]), after=length(successRateGraphBOT))
    }
  }
  
} # End play of game
  
  ###################################################
  ## Data Aggregation between simulation runs
  ###################################################

  ###################################################
  ## Data Aggregation: General statsistics
  ###################################################

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
    urnTotalsDistribution <- round(urnTotals[2] / sum(urnTotals[2]), digits=3) # Create a table with the percentages for the ball totals
    resultsTable <- data.frame(urnTotals, urnTotalsDistribution, reliability1Vector, reliability2Vector) # Combine the two tables into resultsTable
    colnames(resultsTable) <- c("Agent", "Count", "Frequency", "Reliability (w/ Nature)", "Reliability (w/ Agents)") # Name the columns
    # print(resultsTable)
    # Compile the frequency of reinforcement on Nature
    reinfOnNatureVector <- append(reinfOnNatureVector, urnTotalsDistribution[1,1], after=length(reinfOnNatureVector))
    # Compile the success rate of reinforcements for each agent
    successRate <- append(successRate, sum(urnTotals[,2])/(t*r), after=length(successRate))
    # Complile the individual agent's reinforcement frequencies 
    # into a table of the whole population's reinforcement frequencies
    populationResultsTable[2:3] <- (populationResultsTable[2:3] + resultsTable[2:3]) # Add all the resultsTables together to get the populationResultsTable 
  }
  
  # Create the final populationResultsTable
  populationResultsTable[3] <- (round(populationResultsTable[3] / N, digits=3))
  populationResultsTable[,"Reliability (Nature)"] <- reliability1Vector # Add the reliability values to the right column
  populationResultsTable[,"Reliability (Agents)"] <- reliability2Vector # Add the reliability values to the right column
  populationResultsTable[,"Reinf on N"] <- c(NA, reinfOnNatureVector) # Add the reinforcment on nature values to the right hand column
  populationResultsTable[,"Success rate"] <- c(NA, successRate) # Add the success rate value to the right hand column
  
  # This store the results of multiple simulations in one data frame
  simulationResultsData <- rbind(simulationResultsData, populationResultsTable[1:N+1,])

  ###################################################
  ## Data Aggregation: Success rates over time
  ###################################################
  
  # Aggregate the mean success rate of the population
  for (i in 1:(t/(successRateInterval))) {
    df[q,i] <- mean(successRateGraph[(1+((i-1)*N)):(i*N)])
  }
  
  # Aggregate the mean success rate of top quartile of reliable agents
  numAgentsTOP <- sum(reliability1Vector > .75, na.rm=TRUE) # Count the number of reliable agents in the population
  if (numAgentsTOP > 0) {
    for (i in 1:(t/(successRateInterval))) {
      df.top[q,i] <- mean(successRateGraphTOP[(1+((i-1)*numAgentsTOP)):(i*numAgentsTOP)])
    }
  }
  
  # Aggregate the mean success rate of bottom quartile of reliable agents
  numAgentsBOT <- sum(reliability1Vector < .25, na.rm=TRUE) # Count the number of unreliable agents in the population
  if (numAgentsBOT > 0) {
    for (i in 1:(t/(successRateInterval))) {
      df.bottom[q,i] <- mean(successRateGraphBOT[(1+((i-1)*numAgentsBOT)):(i*numAgentsBOT)])
    }
  }
  
  # Reset the successRateGraph vector for the next run of the simulation
  successRateGraph <- c()
  successRateGraphTOP <- c()
  successRateGraphBOT <- c()
  
###################################################  
} ## End of all simulation runs
###################################################


  ###################################################
  ## Graph success rates over time
  ###################################################
  
  # Create the mean success rate vector
  df <- df-N # Correct for initial balls in urns
  print(df)
  data <- c()
  for (i in 1:t) {
  data[i] <- mean(df[,i])
  }
  norm <- c(1:t)*r
  norm.data <- data/norm
  print(norm.data)
  
  # Create the mean success rate vector of the TOP quartile
  df.top <- df.top-N # Correct for initial balls in urns
  print(df.top)
  data.top <- c()
  for (i in 1:t) {
    data.top[i] <- mean(df.top[,i], na.rm = TRUE)
  }
  norm.data.top <- data.top/norm
  print(norm.data.top)
  
  # Create the mean success rate vector of the BOTTOM quartile
  df.bottom <- df.bottom-N # Correct for initial balls in urns
  print(df.bottom)
  data.bottom <- c()
  for (i in 1:t) {
    data.bottom[i] <- mean(df.bottom[,i], na.rm = TRUE)
  }
  norm.data.bottom <- data.bottom/norm
  print(norm.data.bottom)
  
  # Creat the generations vector
  generations <- (1:t)
  
  # Combine the data frames for the three vectors {Population Mean, Top Quartile, Bottom Quartile}
  df.compare <- data.frame(x=generations, y1=norm.data, y2=norm.data.top, y3=norm.data.bottom)
  dm  <- melt(df.compare, id.var = 1)
  
  # Graph the scatterplot of success rates
  ggplot(data = dm, aes(x, value, colour = factor(variable, labels=c("Population Mean","Top Quartile","Bottom Quartile")))) +
    geom_point(alpha=1) +
    scale_fill_discrete(labels=c("Population Mean","Top Quartile", "Bottom Quartile")) +
    labs(x="Generations of Play", y="Success Rate", color="Legend")


  ###################################################
  ## Graphing reliability & consultaton scatterplots
  ###################################################
  
  # Create a scatterplot of agent reliability vs. frequency of reinforcement
  rel <- c(simulationResultsData[2:nrow(simulationResultsData),4])
  reinf <- c(simulationResultsData[2:nrow(simulationResultsData),3])
  par(xpd=FALSE)
  plot(rel, reinf, 
       xlim = c(0,1), ylim = c(0,.4),
       type="p", xlab="Agent Epistemic Reliability", 
       ylab="Frequency of Being Consulted by Population", 
       main="Epistemic Reliability and Frequency of \nBeing Consulted by Population"
       )
  
  # Create a scatterplot of agent social reliability vs. frequency of reinforcement
  if (perfectCommunication==FALSE) {
    socRel <- c(simulationResultsData[2:nrow(simulationResultsData),5])
    reinf <- c(simulationResultsData[2:nrow(simulationResultsData),3])
    par(xpd=FALSE)
    plot(socRel, reinf, 
         xlim = c(.5,1), ylim = c(0,.2),
         type="p", xlab="Agent Social Reliability", 
         ylab="Frequency of Being Consultd by Population", 
         main="Social Reliability and Frequency of \nBeing Consulted by Population"
         )
  }
  
  # Create a scatterplot of agent reliability vs. frequency of consultation with Nature
  par(xpd=FALSE)
  rel <- c(simulationResultsData[2:nrow(simulationResultsData),4])
  reinfN <- c(simulationResultsData[2:nrow(simulationResultsData),6])
  plot(rel, reinfN, 
       xlim = c(0,1), ylim = c(0,1),
       type="p", xlab="Agent Epistemic Reliability", 
       ylab="Frequency of Consulting with Nature", 
       main="Epistemic Reliability \nand Consulting with Nature"
       )
  
  # Create a scatterplot of social agent reliability vs. frequency of consultation with Nature
  if (perfectCommunication==FALSE) {
    par(xpd=FALSE)
    socRel <- c(simulationResultsData[2:nrow(simulationResultsData),5])
    reinfN <- c(simulationResultsData[2:nrow(simulationResultsData),6])
    plot(socRel, reinfN, 
         xlim = c(.5,1), ylim = c(0,1),
         type="p", xlab="Agent Social Reliability", 
         ylab="Frequency of Consulting with Nature", 
         main="Social Reliability \nand Consulting with Nature"
         )
  }

  ###################################################
  ## General population summary statistics
  ###################################################
  
  # Print the simulation's parameters
  print(paste("Simulation parameters: Population size = ", N, 
              ", Generations = " , t, ", Rounds = ", r, 
              ", perfect communication = ", perfectCommunication,
              sep=""))
  
  # Summarize overall results
  summary(simulationResultsData)
  
  # Epistemic Reliability
  # Select & Summarize top quartile w.r.t Reliability
  simulationResults.Reliability.Top <- subset(simulationResultsData, simulationResultsData[4] > .75)
  summary(simulationResults.Reliability.Top)
  
  # Select & Summarize bottom quartile w.r.t Reliability
  simulationResults.Reliability.Bottom <- subset(simulationResultsData, simulationResultsData[4] <= .25)
  summary(simulationResults.Reliability.Bottom)
  
  # Social Reliability
  # Select & Summarize top quartile w.r.t Reliability
  simulationResults.SocialReliability.Top <- subset(simulationResultsData, simulationResultsData[5] >= .875)
  summary(simulationResults.SocialReliability.Top)
  
  # Select & Summarize bottom quartile w.r.t Reliability
  simulationResults.SocialReliability.Bottom <- subset(simulationResultsData, simulationResultsData[5] <= .625)
  summary(simulationResults.SocialReliability.Bottom)
  
  
  ###################################################
  ## Read success rate data
  ###################################################    
  
#   # Read success rate data for the 20-agent population
#   data20 <- read.table("/Users/aydin/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Self-Assembling Games/RL | R/data/SRGraph.PC.N20.S100.t200.r100.csv",
#                                       header = TRUE,
#                                       sep = ",",
#                                       strip.white = TRUE,
#                                       na.strings = "NA",
#                                       fill = TRUE)
# 
#   # Read success rate data for the 5-agent population
#   data05 <- read.table("/Users/aydin/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Self-Assembling Games/RL | R/data/SRGraph.PC.N5.S100.t200.r100.csv",
#                        header = TRUE,
#                        sep = ",",
#                        strip.white = TRUE,
#                        na.strings = "NA",
#                        fill = TRUE) 
  
  
  ###################################################
  ## Plot success rate data
  ###################################################
  
  # Plot average success rate (for every generation of play)
  pdf(width=8, height=6, file = "outPoint.pdf")
  plot(data20$generations, data20$norm.data,
       xlim = c(0,200), ylim = c(.2,.8),
       type="p",
       pch=1,
       xlab="Generations of Play", ylab="Population Mean Success Rate",
       main="Population Mean Success Rate Over Time (Perfect Communication)")
  par(new=TRUE)
  plot(data05$generations, data05$norm.data,
       xlim = c(0,200), ylim = c(.2,.8),
       type="p",
       pch=2,
       axes = FALSE, xlab = "", ylab = "")
  legend("bottomright",
         c("20 Agent","5 Agent"),
         pch=c(1,2))
  dev.off()

  # Plot average success rate (for every 50 generations of play)
  pdf(width=8, height=6, file = "outLine.pdf")
  generationsBrev <- c(0,50,100,150,200)
  data20normBrev <- c(data20$norm.data[1],data20$norm.data[50],data20$norm.data[100],data20$norm.data[150],data20$norm.data[200])
  data05normBrev <- c(data05$norm.data[1],data05$norm.data[50],data05$norm.data[100],data05$norm.data[150],data05$norm.data[200])
  plot(generationsBrev, data20normBrev,
       xlim = c(0,200), ylim = c(.2,.8),
       type="l",
       lt=5,
       axes = FALSE,
       xlab="Generations of Play", ylab="Population Mean Success Rate",
       main="Population Mean Success Rate Over Time (Perfect Communication)")
  par(new=TRUE)
  plot(generationsBrev, data05normBrev,
       xlim = c(0,200), ylim = c(.2,.8),
       type="l",
       lty=1,
       axes = TRUE, xlab = "", ylab = "")
  legend("bottomright", .72, # places a legend at the appropriate place
         c("5 Agent","20 Agent"), # puts text in the legend
         lty=c(1,5), # gives the legend appropriate symbols (lines)
         lwd=c(1,1)) # gives the legend lines the correct color and width
  dev.off()
  
  ###################################################
  # Success rate covariance with individual vs. group reliability
  ###################################################
  
  # Get vector of individual success rates
  IndividualSuccesRate <- simulationResultsData$Success.rate
  # Get vector of individual epistemic reliability values
  IndividualEpistemicReliability <- simulationResultsData$Reliability..Nature.
  # Get vector of individual social reliability values
  IndividualSocialReliability <- simulationResultsData$Reliability..Agents.
  
  # Create the vector of the most reliable agents
  mostReliableAgents <- c()
  for (i in 1:S) {
    mostReliableAgents[(1+(N*(i-1))):(N*i)] <- rep(max(IndividualEpistemicReliability[(1+(N*(i-1))):(N*i)], na.rm = FALSE), N)
  }
  
  # Calculate the covariances & correlations of EPISTEMIC reliability & SUCCESS
  cor(IndividualEpistemicReliability,IndividualSuccesRate)
  # Calculate the covariances & correlations of SOCIAL reliability & SUCCESS
  cor(IndividualSocialReliability,IndividualSuccesRate)
  # Calculate the correlation of THE MOST RELIABLE INDIVIDUAL & SUCCESS
  cor(mostReliableAgents, IndividualSuccesRate)
  
  
  ###################################################
  ## Saving data
  ###################################################
  
  # Set the working directory as you please  
  setwd("~/Desktop")
  
  #   # Save the reliability and consultation frequency data to a CSV file
  if (perfectCommunication==TRUE) {
    Com <- "PC"
  } else {
    Com <- "IC"
  }
  write.csv(simulationResultsData, file = paste("GeneralData.",Com,".N",N,".S",S,".t",t,".r",r,".csv",sep=""))

  # Save the success rate time series data to a CSV file
  colnames(df.compare) <- c("generations", "Population Average", "Top Quartile", "Bottom Quartile")
  write.csv(df.compare, file = paste("SusccessRateData.",Com,".N",N,".S",S,".t",t,".r",r,".csv",sep=""))
  
  ###################################################
  ## Uploading saved general data
  ###################################################
  
  # # Read CSV file, and save to simulationResultsData
  # simulationResultsData <- read.table("~/Desktop/Data/GeneralData.IC.N20.S100.t200.r100.csv",
  #                                     header = TRUE,
  #                                     sep = ",",
  #                                     strip.white = TRUE,
  #                                     na.strings = "NA",
  #                                     fill = TRUE)
  # simulationResultsData <- simulationResultsData[-1] # To eliminate extra first column added by .CSV file
  # colnames(simulationResultsData)
  
  ###################################################
  ## Uploading saved success rate data
  ###################################################    
  
    # Read success rate data for the 20-agent population
    data20 <- read.table("~/Desktop/Data/SusccessRateData.PC.N20.S100.t200.r100.csv",
                                        header = TRUE,
                                        sep = ",",
                                        strip.white = TRUE,
                                        na.strings = "NA",
                                        fill = TRUE)

    # Read success rate data for the 5-agent population
    data05 <- read.table("~/Desktop/Data/SusccessRateData.PC.N5.S100.t200.r100.csv",
                         header = TRUE,
                         sep = ",",
                         strip.white = TRUE,
                         na.strings = "NA",
                         fill = TRUE)
  
###################################################
## End of Document
################################################### 
  