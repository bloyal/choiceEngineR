#Test Run
source("~/GitHub/choiceEngineR/queryChoiceGraph.R");


#---------Test run--------
testRun <- function(graph){
  #Create new session
  session <- createSession(graph);
  print(paste("Starting Session ",session$sessionId, sep=""));
  
  #Initialize on two random options
  options<-getRandomOptionNodes(graph,2); #Returns option nodes
  
  #Initialize temporary variables
  featureScores <- list();
  choice <- list();
  topTenScoreData<-data.frame();
  topTenMSEData<-data.frame();
  solutionCheck <- FALSE;
  i<-0;
  
  #Continue loop until solution is found
  while (solutionCheck == FALSE) {
    #Print cycle number to console
    i<-i+1;
    print(paste("Cycle ",i,sep=""));
    
    #Assign previous choice
    previousChoice <- choice;
    
    #Prompt user to choose between 2 options
    selectionCode <- getSelectionCode(options)
    
    #Pull choice node
    print("Saving choice");
    choice<-options[[selectionCode]];
    
    #Pull non-choice node
    nonChoice<-options[[getNonSelectionCode(selectionCode)]];
    
    print(paste("Choice is ",choice$name, sep=""));
    print(paste("Choice is not ",nonChoice$name, sep=""));
    
    #Save choice node to session
    print("Saving choice to session");
    saveChoiceNodeToSession(graph, session, i, previousChoice, choice);
    
    #Store positive preference scores in graph
    print("Saving positive preference info");
    chosenFeatures<-getOptionNodeDifference(choice, nonChoice);
    assignMultipleFeaturePreferencesToSession(graph, session, chosenFeatures, 1);
    
    #Store negative preference scores in graph
    print("Saving negative preference info");
    nonChosenFeatures<-getOptionNodeDifference(nonChoice, choice);
    assignMultipleFeaturePreferencesToSession(graph, session, nonChosenFeatures, -1)

    #get next two options
    print("Retrieving next options");
    options <- getNextOptions(graph, session);

    #check and see if we're finished
    print("Checking for solution");
    solutionCheck <- getValidSolutionStatus(graph, session, i);
  }
print(paste("May we suggest ", choice$name, "?", sep=""))
}