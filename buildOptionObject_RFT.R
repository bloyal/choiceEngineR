# buildOptionObject_RFT.R
library(rvest);

url<-"http://www.riverfronttimes.com/events/search/date:[2015-01-11]//perPage:500/"
# getHTML(url);

getHTML <- function(url){
  html(url);
}

scrapeRftEvents <- function(html, date, requestDelay = 1){
  
  root<-"http://www.riverfronttimes.com";
  
  mainHTML <- getRftDateHome("2015-01-11");
  #Pull event links out of main date page
  eventURLs <- getEventUrls(mainHTML, root);
  
  eventObject <- list();
  for (i in 1:length(eventUrls)){
    eventObject[[i]] <- getEventObject(eventUrls[i], date);
  }
  
}

getRftDateHome <- function(date, maxEvents = 500){
  url<- paste("http://www.riverfronttimes.com/events/search/date:[",
              date, "]//perPage:",maxEvents,"/", sep="");
  getHTML(url);
}

getEventUrls <- function(html, root){
  #Pull event links out of main event page
  eventURLs <- html %>% html_nodes("h3 a") %>% html_attr("href");
  eventURLs <- paste(root, eventURLs, sep="");
}

getEventObject <- function(url, date){
  eventHTML <- getHTML(url);
  
  name <- eventHTML %>% html_nodes("h1") %>% html_text();
  
  description <- eventHTML %>% 
                  html_nodes("p.description") %>% 
                  html_text() %>% 
                  cleanRftDescription();
  
  provider <- eventHTML %>% html_nodes(".org") %>% html_text();
  
  keywords <- eventHTML %>% 
            html_nodes("p.Event_CategoryTree") %>% 
            html_text() %>%
            cleanRftLabels();
  
  labels <- paste(keywords, "Events", sep=",");
  
  creatorUid <- "bloyal";
  
  creationDt <- Sys.time();
  
}

cleanRftDescription <- function(description){
  #Remove quotation marks
  gsub("[\'\"]","", description);

}

cleanRftLabels <- function(labels){
  desc <- gsub(" \\| ", ",", labels);
  
  #Remove spaces
  gsub("\\s", "", desc);
}