#queryChoiceGraph.R

library(RNeo4j);

source("~/GitHub/choiceEngineR/scoreOptions.R");
source("~/GitHub/choiceEngineR/solutionCheck.R");

#graph = startGraph("http://localhost:7474/db/data/");

getOptionNodeByName <- function(graph, name){
  getLabeledNodes(graph, "Option", name = name)[[1]];
}

getOptionNodeById <- function(graph, optionId){
  getLabeledNodes(graph, "Option", optionId = optionId)[[1]];
}

getFeatureNodeByName <- function(graph, name){
  getLabeledNodes(graph, "Feature", name = name)[[1]];
}

getFeatureNodeById <- function(graph, featureId){
  getLabeledNodes(graph, "Feature", featureId = featureId)[[1]];
}

getMaxSessionId <- function(graph){
  query <- "MATCH (n:Session) RETURN MAX(n.sessionId) as max_id"
  count<-as.integer(cypher(graph, query))
  results<- if(is.na(count)) 0 else count; 
}

createSession <- function(graph){
  node <- createNode(graph, "Session", sessionId = getMaxSessionId(graph)+1)
}

getSessionNodeById <- function(graph, sessionId){
  getLabeledNodes(graph, "Session", sessionId = sessionId);
}

getFeatureNamesByOptionName <- function(graph, name){
  query <- "MATCH (a:Option {name:{name}})-[r:HAS_FEATURE]->(b:Feature)
            RETURN DISTINCT b.name
            ORDER BY b.name";
  results<-cypher(graph, query, name=name);  
  names(results)<-c("name");
  results;
}

getFeatureIdsByOptionId <- function(graph, optionId){
#  print(paste("Getting Features of option",optionId));
  query <- "MATCH (a:Option {optionId:{optionId}})-[r:HAS_FEATURE]->(b:Feature)
            RETURN DISTINCT b.featureId
            ORDER BY b.featureId";
  results<-cypher(graph, query, optionId = optionId);  
  names(results)<-c("featureId");
  results;
}

getRandomOptions <- function(graph, maxItems=1){
  query <- paste("MATCH (a:Option) 
            WITH a, rand() as r 
            RETURN a.name, a.optionId
            ORDER BY r 
            LIMIT ", maxItems, sep="");
  results<-cypher(graph, query);  
  names(results)<-c("name", "optionId");
  results;
}

getRandomOptionNodes <- function(graph, maxItems = 1){
  query <- paste("MATCH (a:Option) 
            WITH a, rand() as r 
            RETURN a
            ORDER BY r 
            LIMIT ", maxItems, sep="");
  getNodes(graph, query);  
}

getAllRelatedOptions <- function(graph, optionId){
  query <- "MATCH (a:Option {optionId:{optionId}})-->(b:Feature)<--(c:Option)
            RETURN DISTINCT c.name, c.optionId
            ORDER BY c.name";
  results<-cypher(graph, query, optionId=optionId);  
  names(results)<-c("name", "optionId");
  results;
}

getRandomRelatedOptions <- function(graph, optionId, maxItems = 2) {
  query <- paste("MATCH (a:Option {optionId:{optionId}})-->(b:Feature)<--(c:Option)
            WITH DISTINCT c 
            WITH c, rand() as r 
            RETURN c.name, c.optionId
            ORDER BY r 
            LIMIT ", maxItems, sep="")
  results<-cypher(graph, query, name=itemName);  
  names(results)<-c("name","optionId");
  results;
}

#Get features of menu item 1 that are NOT in menu item 2
getOptionDifference <- function(graph, optionId_1, optionId_2){
  features1 <- getFeatureIdsByOptionId(graph, optionId_1);
  features2 <- getFeatureIdsByOptionId(graph, optionId_2);
  '%nin%' <- Negate('%in%');
  features1$featureId[features1$featureId %nin% features2$optionId_1];
}

#get features of option 1 that are NOT associated with option 2
getOptionNodeDifference <- function(optionNode1, optionNode2){
  features1 <- unlist(strsplit(optionNode1$keywords,","))
  features2 <- unlist(strsplit(optionNode2$keywords,","))
  '%nin%' <- Negate('%in%');
  features1[features1 %nin% features2];
}

#Create path that starts with session node and travels through each choice in order. Also
#creates a "FINAL_CHOICE" relationship between the session node and the last choice to make
#reporting easier
saveChoicePathToSession <- function(graph, sessionNode, choiceIteration, previousOptionId, optionId) {
  if(choiceIteration==1) {
    choiceNode <- getOptionNodeById(graph, optionId);
    createRel(sessionNode, "MADE_CHOICE", choiceNode, choiceId=1);
    createRel(sessionNode, "LAST_CHOICE", choiceNode);
  }
  else {
    choiceNode <- getOptionNodeById(graph, optionId);
    previousChoiceNode <- getOptionNodeById(graph, previousOptionId);
    createRel(previousChoiceNode, "MADE_CHOICE", choiceNode, choiceId=choiceIteration);
    
    lastChoiceQuery <- "MATCH (s:Session {sessionId:{sessionId}})-[r:LAST_CHOICE]->() RETURN r";
    rel = getSingleRel(graph, lastChoiceQuery, sessionId=sessionNode$sessionId);
    delete(rel);
    createRel(sessionNode, "LAST_CHOICE", choiceNode, sessionId=sessionNode$sessionId);
  }
}

#Same as above, but uses choice objects to simplify logic
saveChoiceNodeToSession <- function(graph, sessionNode, choiceIteration, previousChoiceNode, choiceNode) {
  if(choiceIteration==1) {
    createRel(sessionNode, "MADE_CHOICE", choiceNode, choiceId=1);
    createRel(sessionNode, "LAST_CHOICE", choiceNode);
  }
  else {
    createRel(previousChoiceNode, "MADE_CHOICE", choiceNode, choiceId=choiceIteration);
    
    lastChoiceQuery <- "MATCH (s:Session {sessionId:{sessionId}})-[r:LAST_CHOICE]->() RETURN r";
    rel = getSingleRel(graph, lastChoiceQuery, sessionId=sessionNode$sessionId);
    delete(rel);
    createRel(sessionNode, "LAST_CHOICE", choiceNode, sessionId=sessionNode$sessionId);
  }
}

getSelectionCode <- function(options){
  as.numeric(readline(paste("Please select either: \n(1) ",options[[1]]$name, 
                            "\nor (2) ", options[[2]]$name, ": ", sep="")));
}

getNonSelectionCode <- function(selectionCode){
  if (selectionCode == 1) {2}
  else {1};
}

assignFeaturePreferenceToSession <- function(graph, sessionNode, featureId, incrementValue){
  query <- paste(
    "MATCH (s:Session {sessionId:", sessionNode$sessionId, "}), (f:Feature {featureId:", featureId, "}) ", 
    "MERGE (s) -[r:HAS_AFFINITY_FOR]-> (f) ",
    "ON CREATE SET r.score = ", incrementValue, " ",
    "ON MATCH SET r.score = r.score + ", incrementValue,
    sep="");
  #print(query);
  cypher(graph, query);  
}

assignFeaturePreferenceNameToSession <- function(graph, sessionNode, featureName, incrementValue){
  query <- paste(
    "MATCH (s:Session {sessionId:{sessionId}}), (f:Feature {name:{name}}) ", 
    "MERGE (s) -[r:HAS_AFFINITY_FOR]-> (f) ",
    "ON CREATE SET r.score = ", incrementValue, " ",
    "ON MATCH SET r.score = r.score + ", incrementValue,
    sep="");
  cypher(graph, query, sessionId=sessionNode$sessionId, name=featureName);  
}

assignMultipleFeaturePreferencesToSession <- function(graph, session, features, incrementValue){
  #print("Assigning multiple feature preferences to session");
  #sapply(features, function(feature) assignFeaturePreferenceToSession(graph, session, feature, incrementValue));  
  sapply(features, function(feature) assignFeaturePreferenceNameToSession(graph, session, feature, incrementValue));  
}

addOrIncrementList <- function(elements, list, incrementValue){ 
  '%nin%' <- Negate('%in%');
  for (i in 1:length(elements)){
    #check if element is in list
    if (elements[i] %nin% names(list)){
      list[[elements[i]]] <- incrementValue;
    }
    else {
      list[[elements[i]]] <- list[[elements[i]]] + incrementValue;
    }
  }
  list;
}

getNextOptions <- function(graph, session){
#  getRandomOptionNodes(graph,2); #Returns 2 random option nodes
  getTopOptionNodes(getTopOptionInfo(graph, session, 10)); #Returns 2 options by score and mse
}

getValidSolutionStatus <- function(graph, session, cycle){
#  doesValidSolutionExistBasic(cycle);
  topOptions <- getTopOptionInfo(graph, session, 10);
  doesValidSolutionExist(topOptions, cycle);
}