# buildOptionObject_RFT.R
library(rvest);

url<-"http://www.riverfronttimes.com/events/search/date:[2015-01-11]//perPage:500/"
# getHTML(url);

getHTML <- function(url){
  html(url);
}

scrapeRftEvents <- function(html, requestDelay = 1){
  
  root<-"http://www.riverfronttimes.com";
  
  #Pull event links out of main event page
  eventURLs <- html %>% html_nodes("h3 a") %>% html_attr("href");
  eventURLs <- paste(root, eventURLs, sep="");
  
}