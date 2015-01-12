# buildOptionObject_RFT.R
library(rvest);
library(stringr);

#url<-"http://www.riverfronttimes.com/events/search/date:[2015-01-11]//perPage:500/"
#url2<-"http://www.riverfronttimes.com/events/cirque-du-soleil-varekai-2533361/"
#getHTML(url);

getHTML <- function(url){
  html(url);
}

scrapeRftEvents <- function(date, 
                            root = "http://www.riverfronttimes.com", 
                            requestDelay = 1){
  
  #download event html for specified date
  mainHTML <- getRftDateHome(date);
  Sys.sleep(requestDelay);
  
  #Pull event links out of main date page
  eventURLs <- getEventUrls(mainHTML, root);
  
  createMultipleRftEventObjects(eventURLs, date, requestDelay);
}

createMultipleRftEventObjects <- function(eventURLs, date, requestDelay){
    eventObject <- list();
    for (i in 1:length(eventURLs)){
        print(paste("Downloading event", i));
        eventObject[[i]] <- getEventObject(eventURLs[i], date);
        Sys.sleep(requestDelay);
    }
    eventObject;
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
  eventHTML <- getHTML(url)
  
  list(
    name = eventHTML %>% html_nodes("h1") %>% html_text(),
    description = eventHTML %>% 
                    html_nodes("p.description") %>% 
                    html_text() %>% 
                    cleanRftDescription(),
    provider = eventHTML %>% html_nodes(".org") %>% html_text(),
    keywords = eventHTML %>% 
              html_nodes("p.Event_CategoryTree") %>% 
              html_text() %>%
              cleanRftLabels(),
    labels = paste(eventHTML %>% 
                     html_nodes("p.Event_CategoryTree") %>% 
                     html_text() %>%
                     cleanRftLabels(), "Event", sep=","),
    creatorUid = "bloyal",
    creationDt = Sys.time(),
    optionDt = date,
    imageSrc = eventHTML %>% 
                  html_nodes(".event_article img.framed.photo") %>% 
                  html_attr("src"),
    location = list(
        locationName = eventHTML %>% html_nodes(".org") %>% html_text(),
        locationAddressStreet = eventHTML %>% html_nodes(".address .street-address") %>% html_text(),
        locationAddressLocality = eventHTML %>% html_nodes(".address .locality") %>% html_text(),
        locationAddressRegion= eventHTML %>% html_nodes(".address .region") %>% html_text()
      ),
    measures = list (
      list(
        name = "price",
        value = getRftEventPrice(eventHTML),
        units = "USD"
        )
      )
  );
}

getRftEventPrice <- function(eventHTML){
  price <- eventHTML %>% html_nodes(".event_info p") %>% html_text();
  pattern <- "Price:\\s*\\$(\\d+\\.*\\d*)-*";
  price <- as.numeric(str_match(price, pattern)[1,2]);
  
  if (is.na(price)){
    0;
  }
  else{
    price;
  }
}

cleanRftDescription <- function(description){
  #Remove quotation marks
  gsub("[\'\"]","", description);

}

cleanRftLabels <- function(labels){
  desc <- gsub(" \\| ", ",", labels);
  #Convert multi-word categories into camel-case
  gsub("\\s", "", desc);
}