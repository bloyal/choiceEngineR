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
      date = "1/2/2015",
      imageSrc = "http://ecx.images-amazon.com/images/I/61tDtJTMemL._SY355_.jpg",
      locationName = "Gateway Arch",
      locationAddress = "11 North 4th Street, St. Louis, MO 63102",
      locationGPS = "38.624691,-90.184776",
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

  optionObject <- updateOptionLabels(optionObject); #Update option list to match syntax
  optionObject <- updateKeywords(optionObject); #Add name and description words to keywords
  t <- newTransaction(graph);
  transactionCounter <- 0
  for (i in 1:length(optionObject)){
    transactionCounter <- transactionCounter + 1;
    query <- paste("MATCH (o:Option) 
              WITH count(o) as max_option_id
              CREATE (o:",optionObject[[i]]$labels, " {
              optionId: max_option_id + 1,
              name:{name}, 
              description:{description},
              provider:{provider},
              keywords:{keywords},
              date:{date},
              imageSrc:{imageSrc},
              locationName:{locationName},
              locationAddress:{locationAddress},
              locationGPS:{locationGPS}
              })", sep="");
    appendCypher(t, query, 
                  name = optionObject[[i]]$name, 
                  description = optionObject[[i]]$description,
                  provider = optionObject[[i]]$provider,
                  keywords = optionObject[[i]]$keywords,
                  date = optionObject[[i]]$date,
                  imageSrc = optionObject[[i]]$imageSrc,
                  locationName = optionObject[[i]]$locationName,
                  locationAddress = optionObject[[i]]$locationAddress,
                  locationGPS = optionObject[[i]]$locationGPS
                  );
    if (transactionCounter == transactionMax) {
      commit(t);
      t <- newTransaction(graph);
      transactionCounter <- 0;
    }
  }
  commit(t);
  
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
  library(tm);
  library(reshape);
  library(stringr);
  
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