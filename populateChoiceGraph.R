#populateChoiceGaph.R
#Takes in option object and merges it with choice graph
library(RNeo4j);


getTestOptionObject <- function(){
  newOptions <- list(
    list (
      #required elements
      name = "Red Firetruck",
      description = "The new Red Firetruck from Hasbro provides children with hours of rescue fun.",
      provider = "Hasbro",
      labels = "toys,plastic",
      keywords = "fire, rescue, emergency, plastic, wheels, red",
      #optional elements
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
      measures = list (
        "price" = list (
          "value" = 3.99,
          "unit" = "USD"
          )
        )
      )
    )
}

createOptions <- function(graph, optionObject, transactionMax = 1000){

  optionCount <- length(optionObject);
  optionObject <- updateOptionLabels(optionObject); #Update option list to match syntax
  optionObject <- updateKeywords(optionObject); #Add name and description words to keywords
  t <- newTransaction(graph);
  transactionCounter <- 0
  for (i in 1:optionCount){
    transactionCounter <- transactionCounter + 1;
    query <- paste("CREATE (o:",optionObject[[i]]$labels, " {
              name:{name}, 
              description:{description},
              provider:{provider},
              keywords:{keywords}
              })", sep="");
    print(query);
    appendCypher(t, query, 
                  name = optionObject[[i]]$name, 
                  description = optionObject[[i]]$description,
                  provider = optionObject[[i]]$provider,
                  keywords = optionObject[[i]]$keywords
                  );
    if (transactionCounter == transactionMax) {
      commit(t);
      t <- newTransaction(graph);
      transactionCounter <- 0;
    }
  }
  commit(t);
  
}

updateOptionLabels <- function(optionlist){
  lapply(optionlist, function(x) {x$labels <- combineAllLabels(x$labels); return(x)});
}

combineAllLabels <- function(labelStr, additional = "Option"){
  splitString <- c(additional, unlist(strsplit(labelStr, ",")));
  splitString <- gsub("\\s","", splitString); 
  splitString <- capitalize(splitString);
  paste(splitString, collapse=":");
}

updateKeywords <- function(optionList){
  return(optionList);
}