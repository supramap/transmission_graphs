## TransNet Functions for Shiny
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

###############################{statesConfirmed}
# statesConfirmed(nextStates,charLabelList) returns a true value if each state read in the next record of the 
#     character matrix can be mapped to an integer state that is within the bounds of the character specified
#     (i.e. the integer value i is 1<i<length(characterLabel_specified)). Otherwise it throws an error.
#
statesConfirmed <- function(nextStates,charLabelList){
  returnValue <- TRUE
  for (i in 1:length(charLabelList)) {
    nextState <- nextStates[i]
    nextLabel <- charLabelList[[i]]
    isValid <- (nextState <= length(nextLabel))
    if (is.na(isValid)) {
      isValid <- FALSE
    }
    if(!isValid && !is.na(nextState)) {
      
      cat("ERROR: the state mapping is out of bounds for this character")
      stop()
    }
  }
  return(returnValue)
}

###############################{mapStates}
# mapStates(nextStates,symbols,missing,gap) is a function that maps the next record of character state symbols to
#     integer values. it then returns an integer vector of the mapped states
#
mapStates <- function(nextStates, symbols, missing, gap) {
  mappedStates <- c()
  for (i in 1:length(nextStates)) {
    nextState <- nextStates[i]
    stateMap <- match(nextState,symbols) #returns the first index of the symbol vector where the value matches, else returns NA
    if (is.na(stateMap)) {
      isMissing <- (nextState == missing)
      isGap <- (nextState == gap)
      if (!isMissing) {
        cat("\nERROR: symbol is not an element of the $symbol or $missing sets; the character matrix is erroneous")
        stop()
      }
      if(isGap) {
        cat("\nERROR: gap value should not be used in this state matrix. please check your matrix for gap symbols")
        stop()
      }
    }
    mappedStates <- c(mappedStates,stateMap)
  }
  return(mappedStates)
}

###############################{getMetadata}
#  getMetadata <- function(fileName) is a custom function that collects the metadata from the nexus file and returns it
#     in a list object
#
#     Parameters:
#        
#        fileName - a character vector containing a string literal that is the directory path to the nexus file to be 
#           read in.
#
#     Returns:
#
#        metadata - a list object with the following components:
#           
#           .$charMatrix - a matrix object whose rows are the accession taxa and whose columns are the integer-mapped
#                             character state values
#              attributes:
#                 attr(.,"dimnames") - a list object with 2 character vectors
#                    [[1]] - character vector of accession taxa
#                    [[2]] - character vector of character label descriptions
#
#           .$characterLabels - a list object of n character vectors, each of which corresponds to a different
#                                character (e.g. [[1]] == place, [[2]] == country, etc.)
#              attributes:
#
#                attr(.,"numCharacters") - a numeric vector of length 1 that is the number of character vectors (n)
#
#           .$charSymbolFormatList - a list object of 4 components that reflects the "FORMAT" line in the CHARACTERS
#                                      block of the nexus file
#
#              .$dataType - a character vector of length 1 whose value is the data type of the character block
#              .$gap - character vector whose value is the symbol used to represent a gap in the data
#              .$missing - a character vector whose value is the symbol used to represent missing data
#              .$symbols - a character vector of lenght m whose values are used to create a mapping table
#                             (from character symbol to integer)
#
#           .$taxaData - a character vector of the accession taxa in the tree and character state matrix
#              
#              attributes:
#                 attr(.,"numTaxa") - a numeric vector whose value is the number of taxa in the character vector
#              
#

getMetadata <- function(fileName) {
  
  fileConnection <- file(description = fileName, open = "r") #opens connection to nexus file in "read" mode;
  numCharacters <- 0
  charSymbolFormat <- list()
  charSymbolEnd <- FALSE
  charLabels <- list()
  mappedStates <- c()
  taxaLabelColumn <- c()
  taxaData <- c()
  numTaxa <- c()
  returnList <- list()
  repeat{
    
    nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
    taxaLabelBlockCheck <- grepl("BEGIN TAXA",nextLine)
    charactersBlockCheck <- grepl("BEGIN CHARACTERS",nextLine)
    endMetaData <- grepl("BEGIN TREES",nextLine)
    if (taxaLabelBlockCheck) {
      
      repeat {
        
        nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
        numTaxaCheck <- grepl("DIMENSIONS",nextLine)
        taxaCheck <- grepl("TAXLABELS",nextLine)
        endTaxaBlockCheck <- grepl("END;",nextLine)
        if (numTaxaCheck) {
          nextLine <- strsplit(nextLine,"[^=]+=|;") %>%
            .[[1]] %>%
            .[.!=""] %>%
            as.numeric()
          numTaxa <- nextLine
        }
        if (taxaCheck) {
          
          repeat {
            
            nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
            endTaxa <- grepl(";",nextLine)
            nextLine <- strsplit(nextLine,"\\s|;") %>%
              .[[1]] %>%
              .[.!=""]
            taxaData <- c(taxaData,nextLine)
            if(endTaxa) {
              break
            }
          }
          if(length(taxaData) == numTaxa) {
            
            attr(taxaData,"numTaxa") <- numTaxa   
          } else {
            
            cat("ERROR: the dimensions and the taxa count do not match")
            stop()
          }
          
        }
        if (endTaxaBlockCheck) {
          remove(endTaxaBlockCheck,numTaxaCheck,endTaxa,taxaCheck)
          break
        }
      }
    }
    if (charactersBlockCheck) {
      
      repeat{
        
        nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
        dimensionsCheck <- grepl("DIMENSIONS",nextLine)
        formatCheck <- grepl("FORMAT",nextLine)
        charLabelCheck <- grepl("CHARSTATELABELS",nextLine)
        charMatrixCheck <- grepl("MATRIX",nextLine)
        endCharBlock <- grepl("END;",nextLine)
        if (dimensionsCheck) {
          nextLine <- strsplit(nextLine,"[^=]+=|;") %>%
            .[[1]] %>%
            .[.!=""] %>%
            as.numeric()
          numCharacters <- nextLine
        }
        if (formatCheck) {
          
          formatRegEx <- "\\s|FORMAT|=|\""
          formatString <- strsplit(nextLine,formatRegEx) %>% #the magrittr package is used for shorthand coding where
            .[[1]] %>%                                      # a sequence of functions are acted upon and stored in
            .[.!=""]                                        #the same variable (in this case, formatString)
          for (i in 1:length(formatString)) {
            
            nextToken <- formatString[i]
            dataTypeCheck <- grepl("DATATYPE",nextToken)
            gapCheck <- grepl("GAP",nextToken)
            missingCheck <- grepl("MISSING",nextToken)
            symbolsCheck <- grepl("SYMBOLS",nextToken)
            if (dataTypeCheck) {
              type <- formatString[i+1]
              if (!grepl("STANDARD",type)){
                cat("ERROR: THIS PROGRAM IS SET UP TO READ STANDARD DATATYPES. PLEASE USE A COMPLIANT FILE")
                stop()
              } else {
                charSymbolFormat$dataType <- type
              }
              
            }
            if (gapCheck) {
              charSymbolFormat$gap <- formatString[i+1]
            }
            if (missingCheck) {
              charSymbolFormat$missing <- formatString[i+1]
            }
            if (symbolsCheck) {
              
              repeat {
                
                i <- i+1
                nextToken <- formatString[i]
                confirmSymbol <- grepl("[[:alnum:]]",nextToken)
                if (confirmSymbol){
                  charSymbolFormat$symbols <- c(charSymbolFormat$symbols,nextToken)
                } else {
                  
                  if(nextToken == ";") {
                    charSymbolEnd <- TRUE        
                    break
                  } else {
                    
                    cat("ERROR: ILLEGAL CHARACTER IN SYMBOL DEFINITION: USE ONLY ALPHANUMERIC CHARACTERS AND END WITH ;")
                    stop()
                  }
                  
                }
                
              }
            }
            if (charSymbolEnd) {
              remove(type,symbolsCheck,nextToken,missingCheck,i,gapCheck,formatString,formatRegEx,dimensionsCheck,
                     dataTypeCheck,confirmSymbol,charSymbolEnd)
              break
            }
          }
          
        }
        if (charLabelCheck) {
          
          repeat{
            
            nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
            isLastSet <- grepl(";",nextLine)
            delimRegEx <- "\\s+|(?<![[:alnum:]])/(?![[:alnum:]])|,|;"
            nextLine <- strsplit(nextLine,delimRegEx, perl = TRUE) %>%
              .[[1]] %>%
              .[.!=""]
            index <- as.numeric(nextLine[1])
            charLabels[[index]] <- nextLine[3:length(nextLine)]
            attr(charLabels,"names")[index] <- nextLine[2]
            if (isLastSet) {
              remove(index,isLastSet,delimRegEx)
              break
            }
            
          }
          
        }
        if (charMatrixCheck) {
          
          repeat {
            
            nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
            endMatrix <- grepl(";",nextLine)
            if (!endMatrix) {
              
              nextLine <- strsplit(nextLine,"\\s|;") %>%
                .[[1]] %>%
                .[.!=""]
              if (length(nextLine == 0)) {
                nextTaxon <- nextLine[1]
                nextStates <- nextLine[2] %>%
                  strsplit("") %>%
                  .[[1]] %>%
                  mapStates(charSymbolFormat$symbols,charSymbolFormat$missing,charSymbolFormat$gap)
                taxonConfirmed <- nextTaxon %in% taxaData
                if (taxonConfirmed) {
                  
                  taxaLabelColumn <- c(taxaLabelColumn,nextTaxon)   
                }
                if(statesConfirmed(nextStates,charLabels)) {
                  
                  mappedStates <- c(mappedStates,nextStates)
                }   
              }
              
            } else {
              matrixDimNames <- list(taxaLabelColumn,attr(charLabels,"names"))
              mappedCharMatrix <- matrix(data = mappedStates, ncol = numCharacters, nrow = numTaxa, byrow = TRUE, 
                                         dimnames = matrixDimNames)
              returnList$charMatrix <- mappedCharMatrix
              remove(mappedCharMatrix,taxaLabelColumn,mappedStates,nextStates,nextTaxon,endMatrix)
              break
            }
          }
        }
        if (endCharBlock) {
          remove(taxonConfirmed,formatCheck,charMatrixCheck,charLabelCheck,matrixDimNames,dimensionsCheck,endCharBlock)
          break
        }
      }
    }
    if (endMetaData) {
      remove(taxaLabelBlockCheck,charactersBlockCheck,endMetaData)
      break
    }
  }
  attr(charLabels,"numCharacters") <- numCharacters
  returnList$characterLabels <- charLabels
  returnList$charSymbolFormat <- charSymbolFormat
  returnList$taxaData <- taxaData
  close(fileConnection)
  return(returnList)
}

#\
#  rootedTree structure:
#    list of 3 components:
#      $edge - a numeric matrix with 2 columns. It is the table of edges that describes
#        the phylogenetic tree that was read in by the read.tree() function;
#      $Nnode - a numeric vector of length one whose value is the number of nodes on the 
#        inner branches of the tree;
#      $tip.label - a character vector whose elements are the character string that
#        identifies a leaf node on the phylogenetic tree;
#/


metadataRef <- nexusData$charMatrix[,characterIndex] #CharacterIndex change the number of the character state index of the nexus file you want to use
ref2 <- attr(metadataRef,"names")

#builds a hashmap using the leaf node strings as keys and the character states as values
H <- hashmap(ref2, metadataRef)

# The asr_max_parsimony() function requires a numeric vector that lists the character states
#   of the leaf nodes in sequence as one of its parameter arguments. The following for loop
#   walks through the character vector $tip.label in the rootedTree list, starting at the 
#   index [1], and stores the value of $tip.label[i] in the character vector accession.
#   This character string is then passed into the find function of the hashmap and its
#   character state is returned. Thus when the loop is complete, it has populated 
#   the metadataStates numeric vector with the character states associated with the 
#   leaf nodes in the order that they appeal in $tip.label;
# Get character state for each node that isn't a leaf node (i.e. all the inner nodes)
#asr_max_parsimony accepts 3 parameters:
# - the list object returned by the read.tree() function
# - the character states of the leaf nodes listed in the $tip.label character vector found in
#     the list object
# - the number of possible character states of the trait
# 
# it returns a list object with the following components:
#
#   $ancestral_likelihoods - a numeric matrix object with nrows = the number of inner nodes 
#     in the phylogenetic tree, and ncolumns = to the number of possible character states
#     of the character trait being studied. The value at $ancestral_likelihoods[n,m] is 
#     the probability of interior node n being character state m
#   $success - a logical vector of length one that says whether the process was a success
#               or not
#/
numCharStates <- length(nexusData$characterLabels[[characterIndex]]) ##### change to the number above
ancestralStates = asr_max_parsimony(rootedTree, metadataRef, numCharStates)

# Deletes all keys and values from the hashmap
H$clear()

# Rebuilds hashmap using sequential numbers 1 through the number of leaf nodes as the key/index 
#and using the integer values found in metadataStates as values. It essentially builds a hashmap 
#of the leaf nodes of the tree: their index and their value.
for(i in 1:length(metadataRef)) {
  H$insert(i, metadataRef[i])
}

# Loop through the inner nodes of the phylogenetic tree and assign the most likely character state
# to that tree node;
numLeaves <- length(metadataRef)
numInnerNodes <- rootedTree$Nnode
totalTreeNodes <- numLeaves + numInnerNodes
innerNodeIndices <- (numLeaves+1):totalTreeNodes
numCharacterStates <- length(ancestralStates$ancestral_likelihoods[1,])
counter <- c() #initializes counter vector
for (i in innerNodeIndices) # 474:945  # 473 leaf nodes + 472 inner nodes = 945 total;
{                                                                         
  counter <- ancestralStates$ancestral_likelihoods[i - numLeaves,] #numeric vector of character state 
  # probabilities for inner node of index i
  H$insert(i, match(max(counter), counter)) #enters a new key-value pair 
  #(inner node i -> most likely character state)
}

#after the previous for loop executes, we now have an ASR of the phylogenetic tree given in the beginning.
sourceList <- c()
targetList <- c()

#walk through each edge in the phylogenetic tree. if there's a state change between the two nodes, 
#add the character states to their repspective vector 
#(diedge tail == sourceList, diedge head == targetList)

for(row in 1:nrow(rootedTree$edge)) 
{
  nextEdge <- rootedTree$edge[row,]
  edgeStates <- c(H$find(nextEdge[1]), H$find(nextEdge[2]))
  if (edgeStates[1] != edgeStates[2]) 
  {
    sourceList <- c(sourceList, edgeStates[1])
    targetList <- c(targetList, edgeStates[2])
  }
}

# This creates a table (in the form of a data frame) of the state changes that occur 
#in the phylogenetic tree;
dat <- data.frame(from = sourceList, to = targetList)
#counts the frequency of a specific state change occurring
edges <- count(dat)
names(edges)[names(edges) == "freq"] <- "value"

# Extract the selected metadata state label from the nexusData
metastates <- nexusData$characterLabels[[characterIndex]]

nodes <- data.frame(id = 1:length(metastates), label = metastates) #, fixed = list(x = T, y = T))
igraph.Object <- graph.data.frame(edges,directed = T,vertices = nodes)

indegree <- centr_degree(igraph.Object, mode = c("in")) #Calculates indegree = Destiny of shifts of metadata state for all nodes
outdegree <- centr_degree(igraph.Object, mode = c("out")) #Calculates the Outdegree = Source of shifts of metadata state for all nodes
all.degree <- centr_degree(igraph.Object, mode = c("all")) #Calculates the Degree = Hub, in and out of shifts of metadata state
between.centrality <- betweenness(igraph.Object) #Calculates Betweenness Centrality
closeness.centrality <- closeness(igraph.Object, mode = c("all")) #Calculates Closeness Centrality
sourcehubratio <- outdegree$res/all.degree$res # This is the basic "Source Hub Ratio", still have to work on the normalizing formula

#Create empty matrix and populate with the metrics
outputFileMatrix <- matrix(ncol = 0, nrow = length(metastates)) %>%
  cbind(metastates,all.degree$res,indegree$res,outdegree$res,between.centrality,closeness.centrality,sourcehubratio)# %>%
colnames(outputFileMatrix, do.NULL = FALSE)
colnames(outputFileMatrix) <- c("Metastates","Degree Centrality","Indegree Centrality","Outdegree Centrality","Betweenness Centrality","Closeness Centrality", "Source Hub Ratio") 
