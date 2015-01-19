#Test Run
source("~/GitHub/choiceEngineR/queryChoiceGraph.R");
source("~/GitHub/choiceEngineR/scoreOptions.R");
source("~/GitHub/choiceEngineR/solutionCheck.R");

#---------Test run--------
testRun <- function(graph){
  #Create new session
  session <- createSession(graph);
  print(paste("Starting Session ",session$sessionId, sep=""));
  
  #Initialize on two random options
  options<-getRandomOptionNodes(graph,2); #Returns option nodes
  featureScores <- list();
  choice <- list();
  topTenScoreData<-data.frame();
  topTenMSEData<-data.frame();
  solutionCheck <- FALSE;
  i<-0;
  while (solutionCheck == FALSE) {
    #Print cycle number to console
    i<-i+1;
    print(paste("Cycle ",i,sep=""));
    
    #Assign previous choice
    previousChoice <- choice;
    
    #Prompt user to choose between 2 options
    selectionCode <- getSelectionCode(options)
    
    #Pull choice node
    choice<-options[[selectionCode]];
    
    #Pull non-choice node
    nonChoice<-options[[getNonSelectionCode(selectionCode)]];
    
    print(paste("Choice is ",choice$name, sep=""));
    print(paste("Choice is not ",nonChoice$name, sep=""));
    
    #Save choice node to session
    saveChoiceNodeToSession(graph, session, i, previousChoice, choice);
    
    #Store positive preference scores in graph
    chosenFeatures<-getOptionNodeDifference(choice, nonChoice);
    assignMultipleFeaturePreferencesToSession(graph, session, chosenFeatures, 1);
    
    #Store negative preference scores in graph
    nonChosenFeatures<-getOptionNodeDifference(nonChoice, choice);
    assignMultipleFeaturePreferencesToSession(graph, session, nonChosenFeatures, -1)

    #get next two options
    options<-getRandomOptionNodes(graph,2); #Returns option nodes

    #check and see if we're finished
    solutionCheck <- doesValidSolutionExistBasic(i);
    
#     #print("Getting top option info");
#     topOptions <- getTopOptionInfo(graph, session, 10);
#     topTenScoreData <- rbind(topTenScoreData, topOptions$score);
#     topTenMSEData <- rbind(topTenMSEData, topOptions$mse);
# 
#     print("Top Features are:");
#     print (getTopFeatureInfo(graph, session, 10));
# 
#     print("Top Options are:");
#     print(topOptions[1:5,]);

    #solutionCheck <- doesValidSolutionExist(topOptions, i);
#options <- data.frame(topOptions[1:2,]);    

    
  }
#print(paste("May we suggest ", topOptions[1,"name"], "?", sep=""))
print(paste("May we suggest ", choice$name, "?", sep=""))
}