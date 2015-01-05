#Test Run
source("~/GitHub/choiceEngineR/queryCheesecakeFactoryGraph.R");
source("~/GitHub/choiceEngineR/get_high_scoring_options.R");
source("~/GitHub/choiceEngineR/solutionCheck.R");

#---------Test run--------
#Initialize on 2 random menu items
testRun <- function(graph){
  session <- createSession(graph);
  print(paste("Starting Session ",session$id, sep=""));
  options<-getRandomMenuItemNames(graph,2);
  featureScores <- list();
  choice <- list();
  topTenScoreData<-data.frame();
  topTenMSEData<-data.frame();
  solutionCheck <- FALSE;
  i<-0;
#   for (i in 1:10) {
  while (solutionCheck == FALSE) {
    i<-i+1;
    print(paste("Cycle ",i,sep=""));
    #Save previous choice
    previousChoice <- choice;
#-----Add something here about descriptions-------
    choice<-options[readline(paste("Please select either: \n(1) ",options[1,], 
                                   "\nor (2) ", options[2,], ": ", sep="")),];
    print(paste("Choice is ",choice, sep=""));
    
    #saveChoiceToSession(graph, session, choice);
    saveChoicePathToSession(graph, session, i, previousChoice, choice);
    
    nonChoice<-options[options!=choice];
    #print(paste("Choice is not ",nonChoice, sep=""));
    
    chosenFeatures<-getMenuItemDifference(graph, choice, nonChoice);
    #Use this option to store preference scores in memory
    #featureScores<-addOrIncrementList(chosenFeatures, featureScores, 1)
    #Use this option to store preference scores in database
    assignMultipleFeaturePreferencesToSession(graph, session, chosenFeatures, 1)
    
    nonChosenFeatures<-getMenuItemDifference(graph, nonChoice, choice);
    #Use this option to store preference scores in memory
    #featureScores<-addOrIncrementList(nonChosenFeatures, featureScores, -1)
    #Use this option to store preference scores in database
    assignMultipleFeaturePreferencesToSession(graph, session, nonChosenFeatures, -1)
    
#      if (i < 4) {options <- getSomeRelatedMenuItemNames(graph, choice, 2);}
#      else {options <- getHighScoringOptions(graph, session, choice, 2);}
    
#    options <- getHighScoringOptions(graph, session, choice, 2);

    topOptions <- getTopOptionInfo(graph, session, 10);
    topTenScoreData <- rbind(topTenScoreData, topOptions$score);
    topTenMSEData <- rbind(topTenMSEData, topOptions$mse);

    print("Top Features are:");
    print (getTopFeatureInfo(graph, session, 10));

    print("Top Options are:");
    print(topOptions[1:5,]);

    solutionCheck <- doesValidSolutionExist(topOptions, i);

    #options <- getLowMSEOptions(graph, session, 2);
    options <- data.frame(topOptions[1:2,"menuItem"]);
  }
print(paste("May we suggest ", topOptions[1,"menuItem"], "?", sep=""))
#list("topTenScoreData"=topTenScoreData, "topTenMSEData"=topTenMSEData);
}