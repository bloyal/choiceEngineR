#buildOptionObject_CKF.R
library(rvest);

# url<-"http://www.thecheesecakefactory.com/menu"
# getHTML(url);

getHTML <- function(url){
  html(url);
}

scrapeCkfMenu <- function(html){
  
  #Extract menu item titles based on css class
  menu_titles <- html %>%
    html_nodes(".item-title") %>%
    html_text();
  
  #Extract menu item descriptions based on css class
  menu_descriptions <- html %>%
    html_nodes(".item-description") %>%
    html_text();
  
  #Save titles and descriptions to data frame
  ckf_items <- data.frame(name=menu_titles, description=menu_descriptions);
  
  #This stuff should be taken out in real versions - its just to clean up test data
  #remove IP symbols from titles
  ckf_items$name<-str_replace_all(ckf_items$name,"[™®]","");
  
  #Remove apostrophes from title and descriptions (messes up indexing)
  ckf_items$name<-str_replace_all(ckf_items$name,"'","");
  ckf_items$description<-str_replace_all(ckf_items$description,"'","");
  
  #Remove other weird characters from title and descriptions (Only for testing!)
  ckf_items$name<-str_replace_all(ckf_items$name,"[^[:alnum:]^[:space:]]","");
  ckf_items$description<-str_replace_all(ckf_items$description,"[^[:alnum:]^[:space:]]","");
  
  #Remove duplicate records
  ckf_items<-unique(ckf_items);
  
  #Remove Renees Special
  ckf_items <- ckf_items[ ckf_items$name!="Renees Special",];
  
  #Add provider info and labels
  ckf_items <- cbind(ckf_items, provider=rep("The Cheesecake Factory", nrow(ckf_items)));
  ckf_items <- cbind(ckf_items, labels=rep("Food", nrow(ckf_items)));

  #transform into option object
  object<-list();
  for( i in 1:nrow(ckf_items)){
    a<-list(
      name = ckf_items[[i,1]],
      description = ckf_items[[i,2]],
      provider = ckf_items[[i,3]],
      labels = ckf_items[[i,4]],
      creatorUid = "bloyal",
      creationDt = Sys.time()
      );
    object[[i]]<-a;
  };
  object;
}