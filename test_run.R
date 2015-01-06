#Test Run
source("~/GitHub/choiceEngineR/queryChoiceGraph.R");
source("~/GitHub/choiceEngineR/scoreOptions.R");
source("~/GitHub/choiceEngineR/solutionCheck.R");

#---------Test run--------
#Initialize on 2 random menu items
testRun <- function(graph){
  session <- createSession(graph);
  print(paste("Starting Session ",session$sessionId, sep=""));
  options<-getRandomMenuItems(graph,2);
  featureScores <- list();
  choice <- list();
  topTenScoreData<-data.frame();
  topTenMSEData<-data.frame();
  solutionCheck <- FALSE;
  i<-0;
  while (solutionCheck == FALSE) {
    i<-i+1;
    print(paste("Cycle ",i,sep=""));
    #Save previous choice
    previousChoice <- choice;
    choice<-options[readline(paste("Please select either: \n(1) ",options[1,1], 
                                   "\nor (2) ", options[2,1], ": ", sep="")),];
    print(paste("Choice is ",choice[1], sep=""));
    saveChoicePathToSession(graph, session, i, previousChoice[2], choice[2]);
    nonChoice<-options[options[2]!=choice[2],];
    
    #Store positive preference scores in graph
    chosenFeatures<-getMenuItemDifference(graph, choice[2], nonChoice[2]);
    assignMultipleFeaturePreferencesToSession(graph, session, chosenFeatures, 1);
    
    #Store negative preference scores in graph
    nonChosenFeatures<-getMenuItemDifference(graph, nonChoice[2], choice[2]);
    assignMultipleFeaturePreferencesToSession(graph, session, nonChosenFeatures, -1)

    topOptions <- getTopOptionInfo(graph, session, 10);
    topTenScoreData <- rbind(topTenScoreData, topOptions$score);
    topTenMSEData <- rbind(topTenMSEData, topOptions$mse);

    print("Top Features are:");
    print (getTopFeatureInfo(graph, session, 10));

    print("Top Options are:");
    print(topOptions[1:5,]);

    solutionCheck <- doesValidSolutionExist(topOptions, i);
    options <- data.frame(topOptions[1:2,"optionId"]);
  }
print(paste("May we suggest ", topOptions[1,"name"], "?", sep=""))
}