library(shiny)
library(ape)
library(castor)
library(visNetwork)
library(hashmap)
library(plyr)
library(network)
library(igraph)
library(data.table)
library(magrittr)
source("transnet.R")

server <- function(input, output) {
  
  output$nexustable <- renderText({
    
    req(input$nexus1)
    nexusTree2 <- read.nexus(input$nexus1$datapath)
    nexusData <- getMetadata(input$nexus1$datapath)

    return(nexusData)
  })
  
  ## Input objects
  # nexusTree2 <- read.nexus(input$nexus1$datapath)
  # nexusData <- getMetadata(input$nexus1$datapath)
  # characterIndex <- input$charIndex
  # metadataRef <- nexusData$charMatrix[,characterIndex] #CharacterIndex change the number of the character state index of the nexus file you want to use
  # ref2 <- attr(metadataRef,"names")
  
  # #builds a hashmap using the leaf node strings as keys and the character states as values
  # H <- hashmap(ref2, metadataRef)
  # numCharStates <- length(nexusData$characterLabels[[characterIndex]]) ##### change to the number above
  # ancestralStates = asr_max_parsimony(rootedTree, metadataRef, numCharStates)
  # 
  # # Deletes all keys and values from the hashmap
  # H$clear()
  # 
  # # Rebuilds hashmap using sequential numbers 1 through the number of leaf nodes as the key/index 
  # #and using the integer values found in metadataStates as values. It essentially builds a hashmap 
  # #of the leaf nodes of the tree: their index and their value.
  # for(i in 1:length(metadataRef)) {
  #   H$insert(i, metadataRef[i])
  # }
  # 
  # # Loop through the inner nodes of the phylogenetic tree and assign the most likely character state
  # # to that tree node;
  # numLeaves <- length(metadataRef)
  # numInnerNodes <- rootedTree$Nnode
  # totalTreeNodes <- numLeaves + numInnerNodes
  # innerNodeIndices <- (numLeaves+1):totalTreeNodes
  # numCharacterStates <- length(ancestralStates$ancestral_likelihoods[1,])
  # counter <- c() #initializes counter vector
  # for (i in innerNodeIndices) # 474:945  # 473 leaf nodes + 472 inner nodes = 945 total;
  # {                                                                         
  #   counter <- ancestralStates$ancestral_likelihoods[i - numLeaves,] #numeric vector of character state 
  #   # probabilities for inner node of index i
  #   H$insert(i, match(max(counter), counter)) #enters a new key-value pair 
  #   #(inner node i -> most likely character state)
  # }
  # 
  # #after the previous for loop executes, we now have an ASR of the phylogenetic tree given in the beginning.
  # sourceList <- c()
  # targetList <- c()
  # 
  # #walk through each edge in the phylogenetic tree. if there's a state change between the two nodes, 
  # #add the character states to their repspective vector 
  # #(diedge tail == sourceList, diedge head == targetList)
  # 
  # for(row in 1:nrow(rootedTree$edge)) 
  # {
  #   nextEdge <- rootedTree$edge[row,]
  #   edgeStates <- c(H$find(nextEdge[1]), H$find(nextEdge[2]))
  #   if (edgeStates[1] != edgeStates[2]) 
  #   {
  #     sourceList <- c(sourceList, edgeStates[1])
  #     targetList <- c(targetList, edgeStates[2])
  #   }
  # }
  # 
  # # This creates a table (in the form of a data frame) of the state changes that occur 
  # #in the phylogenetic tree;
  # dat <- data.frame(from = sourceList, to = targetList)
  # #counts the frequency of a specific state change occurring
  # edges <- count(dat)
  # names(edges)[names(edges) == "freq"] <- "value"
  # 
  # # Extract the selected metadata state label from the nexusData
  # metastates <- nexusData$characterLabels[[characterIndex]]
  # 
  # nodes <- data.frame(id = 1:length(metastates), label = metastates) #, fixed = list(x = T, y = T))
  # igraph.Object <- graph.data.frame(edges,directed = T,vertices = nodes)
  # 
  # indegree <- centr_degree(igraph.Object, mode = c("in")) #Calculates indegree = Destiny of shifts of metadata state for all nodes
  # outdegree <- centr_degree(igraph.Object, mode = c("out")) #Calculates the Outdegree = Source of shifts of metadata state for all nodes
  # all.degree <- centr_degree(igraph.Object, mode = c("all")) #Calculates the Degree = Hub, in and out of shifts of metadata state
  # between.centrality <- betweenness(igraph.Object) #Calculates Betweenness Centrality
  # closeness.centrality <- closeness(igraph.Object, mode = c("all")) #Calculates Closeness Centrality
  # sourcehubratio <- outdegree$res/all.degree$res # This is the basic "Source Hub Ratio", still have to work on the normalizing formula
  # 
  # #Create empty matrix and populate with the metrics
  # outputFileMatrix <- matrix(ncol = 0, nrow = length(metastates)) %>%
  #   cbind(metastates,all.degree$res,indegree$res,outdegree$res,between.centrality,closeness.centrality,sourcehubratio)# %>%
  # colnames(outputFileMatrix, do.NULL = FALSE)
  # colnames(outputFileMatrix) <- c("Metastates","Degree Centrality","Indegree Centrality","Outdegree Centrality","Betweenness Centrality","Closeness Centrality", "Source Hub Ratio") 
  
  
  
  ## Outputs
  #output$nexustable <- renderTable(nexusData)
  
  #nexusTree2 <- read.nexus(input$nexus1)
  #nexusData <- getMetadata(input$nexus1)
  #output$nexusData <- nexusData
  
  #charIndex <- input$charIndex
  #characterIndex <- as.numeric(charIndex) #Transforms the input from string to numeric so it can be loaded on metadataRef
  #rootedTree <- nexusTree2

}