#populateChoiceGaph.R
#Takes in option object and merges it with choice graph
library(RNeo4j);
library(tm);
library(reshape);
library(stringr);

buildExampleGraph <- function(){
  newOptions<-getTestOptionObject();
  graph = startGraph("http://localhost:7474/db/data/");
  clear(graph, input=FALSE);
  prepareConstraints(graph);
  createOptions(graph, newOptions);
}

getTestOptionObject <- function(){
  list(
    list (
      #required elements
      name = "Red Firetruck",
      description = "The new Red Firetruck from Hasbro provides children with hours of rescue fun.",
      provider = "Hasbro",
      labels = "toys,plastic",
      keywords = "fire, rescue, emergency, plastic, wheels, red",
      creatorUid = "bloyal",
      creationDt = Sys.time(),
      #optional elements
      OptionDate = "1/2/2015",
      imageSrc = "http://ecx.images-amazon.com/images/I/61tDtJTMemL._SY355_.jpg",
      locations = list(
        list(
          locationName = "Gateway Arch",
          locationAddress = "11 North 4th Street, St. Louis, MO 63102",
          locationGPS = "38.624691,-90.184776"
          )
        ),
      measures = list (
        "price" = list (
          "value"=11.95,
          "unit"="USD"
          ),
        "weight" = list (
          "value" = 1.5,
          "unit" = "grams"
          ),
        "length" = list (
          "value" = 2,
          "unit" = "feet"
          )
        )
      ),
    list(
      name = "Yellow Submarine",
      description = "A favorite from across (and under) the ocean, this yellow submarine is a great place to live, work, and play in!",
      provider = "Playmobile",
      labels = "toys, children",
      keywords = "ocean, underwater, sea, Beatles, submarine, nautical, yellow",
      creatorUid = "bloyal",
      creationDt = Sys.time(),
      locations = list(
        list(
          locationName = "Gateway Arch",
          locationAddress = "11 North 4th Street, St. Louis, MO 63102",
          locationGPS = "38.624691,-90.184776"
        )
      ),
      measures = list(
        "price" = list (
          "value"=15.25,
          "unit"="USD"
        ),
        "volume" = list (
          "value"=12,
          "unit"="oz"
          )
        )
      ),
    list (
      name = "Purple Rain",
      description = "Your child's favorite symbole of love and friendship, Purple Rain is a delicious grape drink, now with 99% vitamin C.is a great place to live, work, and play in!",
      provider = "Nabisco",
      labels = "drink",
      keywords = "prince, yummy, purple, sea, red, music, drink",
      creatorUid = "bloyal",
      creationDt = Sys.time(),
      measures = list (
        "price" = list (
          "value" = 3.99,
          "unit" = "USD"
          )
        )
      )
    )
}

prepareConstraints <- function(graph){
  addConstraint(graph, "Feature", "featureId");
  addConstraint(graph, "Feature", "name");
  addConstraint(graph, "Option", "optionId");
  addConstraint(graph, "Option", "name");
}

createOptions <- function(graph, optionObject, transactionMax = 1000){

  optionObject <- updateOptionLabels(optionObject); #Update option list to match syntax
  optionObject <- updateKeywords(optionObject); #Add name and description words to keywords
  createBulkOptions(graph, optionObject, transactionMax);
  createBulkFeatures(graph, optionObject, transactionMax);
  createBulkLocations(graph, optionObject, transactionMax);
  createOptionFeatureRelationships(graph, optionObject, transactionMax);
}

createBulkOptions <- function(graph, optionObject, transactionMax){
  
  t <- newTransaction(graph);
  transactionCounter <- 0;
  for (i in 1:length(optionObject)){
    transactionCounter <- transactionCounter + 1;
    createOptionNode(t, optionObject[[i]]);
    if (transactionCounter == transactionMax) {
      commit(t);
      t <- newTransaction(graph);
      transactionCounter <- 0;
    }
  }
  commit(t);
  
}

createOptionNode <- function(transaction, option){
  query <- paste("MATCH (o:Option) 
              WITH count(o) as max_option_id
              CREATE (o:",option$labels, " {
              optionId: max_option_id + 1,
              name:{name}, 
              description:{description},
              provider:{provider},
              keywords:{keywords},
              creatorUid:{creatorUid},
              creationDt:{creationDt},
              optionDate:{optionDate},
              imageSrc:{imageSrc}
              })", sep="");
  appendCypher(transaction, query, 
               name = option$name, 
               description = option$description,
               provider = option$provider,
               keywords = option$keywords,
               creatorUid = option$creatorUid,
               creationDt = option$creationDt,
               optionDate = option$optionDate,
               imageSrc = option$imageSrc,
               locationName = option$locationName,
               locationAddress = option$locationAddress,
               locationGPS = option$locationGPS
  );
}

updateOptionLabels <- function(optionList){
  lapply(optionList, function(x) {x$labels <- combineAllLabels(x$labels); return(x)});
}

combineAllLabels <- function(labelStr, additional = "Option"){
  splitString <- c(additional, unlist(strsplit(labelStr, ",")));
  splitString <- gsub("\\s","", splitString); 
  paste(toupper(substring(splitString, 1,1)), substring(splitString, 2),
        sep="", collapse=":")
}

updateKeywords <- function(optionList){
  lapply(optionList, function(x) {x$keywords <- consolidateKeywords(x$name, x$description, x$keywords); return(x)});
}

consolidateKeywords<- function(name, description, keywords) {
  
  #combine names, descriptions, and user-defined keywords
  keywords<-paste(name, description, keywords);
  
  #Process text with tm
  corp <- VCorpus(VectorSource(keywords))
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeWords, stopwords("english"))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, stemDocument)
  keywords<-as.character(unlist(sapply(corp, `[`, "content")), stringsAsFactors=F)
  #Tokenize
  keywords<-lapply(keywords,scan_tokenizer);
  keywords<-lapply(keywords,unique);
  keywords<- paste(unlist(keywords), collapse=",");
}

createBulkFeatures <- function(graph, optionObject, transactionMax){
  
  t <- newTransaction(graph);
  transactionCounter <- 0;
  features <- getConsolidatedFeatureVector(optionObject);
  for (i in 1:length(features)){
    transactionCounter <- transactionCounter + 1;
    createFeatureNode(t, features[i]);
    if (transactionCounter == transactionMax) {
      commit(t);
      t <- newTransaction(graph);
      transactionCounter <- 0;
    }
  }
  commit(t);
  
}

getConsolidatedFeatureVector <- function(optionObject){
  combinedFeatures <- paste(unlist(lapply(optionObject, function(x){x$keywords})),collapse=",");
  combinedFeatures <- unique(unlist(strsplit(combinedFeatures,",")));
}

createFeatureNode <- function(transaction, feature){
  query <- paste("MATCH (f:Feature) 
                 WITH count(f) as max_feature_id
                 CREATE (f:Feature {
                 featureId: max_feature_id + 1,
                 name:{name}
                 })", sep="");
  appendCypher(transaction, query, name = feature);
}

createBulkLocations <- function(graph, optionObject, transactionMax){
  
  t <- newTransaction(graph);
  transactionCounter <- 0;
  locations <- getCondolidatedLocationList(optionObject);
  for (i in 1:length(locations)){
    transactionCounter <- transactionCounter + 1;
    createLocationNode(t, locations[[i]]);
    if (transactionCounter == transactionMax) {
      commit(t);
      t <- newTransaction(graph);
      transactionCounter <- 0;
    }
  }
  commit(t);
  
}

getCondolidatedLocationList <- function(optionObject){
  locations<-unlist(lapply(optionObject, function(x){x$locations}),recursive=FALSE);
  locations<-unique(locations);
}

createLocationNode <- function(transaction, location){
  query <- paste("MATCH (f:Feature) 
                 WITH count(f) as max_feature_id
                 CREATE (f:Feature:Location {
                 featureId: max_feature_id + 1,
                 name:{name},
                  address:{address},
                  gps:{gps}
                 })", sep="");
  appendCypher(transaction, query, 
               name = location$locationName, 
               address=location$locationAddress,
               gps=location$locationGPS);
}

createOptionFeatureRelationships <- function(graph, optionObject, transactionMax){
  
  t <- newTransaction(graph);
  transactionCounter <- 0;
  links <- getOptionFeatureLinks(optionObject);
  for (i in 1:nrow(links)){
    transactionCounter <- transactionCounter + 1;
    createOptionFeatureRelationship(t, links[i,]);
    if (transactionCounter == transactionMax) {
      commit(t);
      t <- newTransaction(graph);
      transactionCounter <- 0;
    }
  }
  commit(t);
}

getOptionFeatureLinks <- function(optionObject){

  links<-lapply(optionObject, function(x) unlist(strsplit(x$keywords,",")));
  names(links)<-unlist(lapply(optionObject, function(x) x$name));
  links<-melt(links);
  names(links)<-c("feature","option");
  links;
}

createOptionFeatureRelationship <- function(transaction, link){

  query <- "MATCH (o:Option {name:{name}}), (f:Feature {name:{feature}}) 
            MERGE (o)-[:HAS_FEATURE {strength:1}]->(f)";
  
  appendCypher(transaction, query, name = link$option, feature = link$feature);
  
}