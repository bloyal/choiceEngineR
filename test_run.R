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
    saveChoicePathToSession(graph, session, i, previousChoice[[2]], choice[[2]]);
    print("Choice saved");
    nonChoice<-options[options[,2]!=choice[,2],];
    print(paste("Non choice is ",nonChoice[1], sep=""));
    
    #Store positive preference scores in graph
    chosenFeatures<-getOptionDifference(graph, choice[[2]], nonChoice[[2]]);
    print("Chosen features found");
    assignMultipleFeaturePreferencesToSession(graph, session, chosenFeatures, 1);
    
    #Store negative preference scores in graph
    nonChosenFeatures<-getOptionDifference(graph, nonChoice[[2]], choice[[2]]);
    assignMultipleFeaturePreferencesToSession(graph, session, nonChosenFeatures, -1)

    print("Getting top option info");
    topOptions <- getTopOptionInfo(graph, session, 10);
    print(topOptions);
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