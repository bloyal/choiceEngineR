#queryCheesecakeFactoryGraph.R
library(RNeo4j);

graph = startGraph("http://localhost:7474/db/data/");

getMenuItemNode <- function(graph, itemName){
  getLabeledNodes(graph, "Food", name = itemName)[[1]];
}

getFeatureNode <- function(graph, featureName){
  getLabeledNodes(graph, "Feature", name = featureName)[[1]];
}

getMaxSessionId <- function(graph){
  query <- "MATCH (n:Session) RETURN MAX(n.id) as max_id"
  count<-as.integer(cypher(graph, query))
  results<- if(is.na(count)) 0 else count; 
}

createSession <- function(graph){
  node <- createNode(graph, "Session", id = getMaxSessionId(graph)+1)
}

getSessionNode <- function(graph, sessionId){
  getLabeledNodes(graph, "Session", id = sessionId);
}

getFeatureNamesForMenuItem <- function(graph, itemName){
  query <- "MATCH (a:Food {name:{name}})-[r:HAS_FEATURE]->(b:Feature)
            RETURN DISTINCT b.name
            ORDER BY b.name";
  results<-cypher(graph, query, name=itemName);  
  names(results)<-c("name");
  results;
}

getRandomMenuItemNames <- function(graph, maxItems=1){
  query <- paste("MATCH (a:Food) 
            WITH a, rand() as r 
            RETURN a.name
            ORDER BY r 
            LIMIT ", maxItems, sep="");
  results<-cypher(graph, query);  
  names(results)<-c("name");
  results;
}

getAllRelatedMenuItemNames <- function(graph, itemName){
  query <- "MATCH (a:Food {name:{name}})-->(b:Feature)<--(c:Food)
            RETURN DISTINCT c.name
            ORDER BY c.name";
  results<-cypher(graph, query, name=itemName);  
  names(results)<-c("name");
  results;
}

getSomeRelatedMenuItemNames <- function(graph, itemName, maxItems = 2) {
  query <- paste("MATCH (a:Food {name:{name}})-->(b:Feature)<--(c:Food)
            WITH DISTINCT c 
            WITH c, rand() as r 
            RETURN c.name 
            ORDER BY r 
            LIMIT ", maxItems, sep="")
  results<-cypher(graph, query, name=itemName);  
  names(results)<-c("name");
  results;
}

#Get features of menu item 1 that are NOT in menu item 2
getMenuItemDifference <- function(graph, name1, name2){
  features1 <- getFeatureNamesForMenuItem(graph, name1);
  features2 <- getFeatureNamesForMenuItem(graph, name2);
  '%nin%' <- Negate('%in%');
  features1$name[features1$name %nin% features2$name];
}

#Create direct link between the session node and every choice - decided not to go this route
saveChoiceToSession <- function(graph, sessionNode, itemName){
    itemNode <- getMenuItemNode(graph, itemName);
    createRel(sessionNode, "MADE_CHOICE", itemNode, 
              id=getSessionChoicesCount(graph, sessionNode)+1);
}
getSessionChoicesCount <- function(graph, sessionNode){
  choices <- outgoingRels(sessionNode, "MADE_CHOICE");
  if (is.null(choices)) 0 else length(choices);
}

#Create path that starts with session node and travels through each choice in order. Also
#creates a "FINAL_CHOICE" relationship between the session node and the last choice to make
#reporting easier
saveChoicePathToSession <- function(graph, sessionNode, choiceIteration, previousChoice, choice) {
  if(choiceIteration==1) {
    choiceNode <- getMenuItemNode(graph, choice);
    createRel(sessionNode, "MADE_CHOICE", choiceNode, id=1);
    createRel(sessionNode, "LAST_CHOICE", choiceNode);
  }
  else {
    choiceNode <- getMenuItemNode(graph, choice);
    previousChoiceNode <- getMenuItemNode(graph, previousChoice);
    createRel(previousChoiceNode, "MADE_CHOICE", choiceNode, id=choiceIteration);
    
    lastChoiceQuery <- "MATCH (s:Session {id:{id}})-[r:LAST_CHOICE]->() RETURN r";
    rel = getSingleRel(graph, lastChoiceQuery, id=sessionNode$id);
    delete(rel);
    createRel(sessionNode, "LAST_CHOICE", choiceNode, session_id=sessionNode$id);
  }
}
assignFeaturePreferenceToSession <- function(graph, sessionNode, featureName, incrementValue){
  query <- paste(
    "MATCH (s:Session {id:", sessionNode$id, "}), (f:Feature {name:'", featureName, "'}) ", 
    "MERGE (s) -[r:HAS_AFFINITY_FOR]-> (f) ",
    "ON CREATE SET r.score = ", incrementValue, " ",
    "ON MATCH SET r.score = r.score + ", incrementValue,
    sep="");
  cypher(graph, query);  
}

assignMultipleFeaturePreferencesToSession <- function(graph, session, features, incrementValue){
  sapply(features, function(feature) assignFeaturePreferenceToSession(graph, session, feature, incrementValue))  
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