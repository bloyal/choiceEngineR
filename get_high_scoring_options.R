#get high scoring options related to a node

getHighScoringOptions <- function(graph, session, itemName, maxItems = 2){
  #print(paste("Item Name is ",itemName,sep=""))
  query <- paste("MATCH (a:Food {name:{name}})-->(f:Feature)<--(c:Food),
    (s:Session {id:", session$id, "})-[r:HAS_AFFINITY_FOR]->(f)
    WITH a.name as choice_name, c.name as related_item_name, sum(r.score) as affinity_score
    WITH choice_name, related_item_name, affinity_score, rand() as random
    RETURN related_item_name
    ORDER BY choice_name, affinity_score DESC, random
    LIMIT ", maxItems, sep="");
  
  #print(query)
  results<-cypher(graph, query, name=itemName);  
  names(results)<-c("name");
  results;
}

getLowMSEOptions <- function(graph, session, maxItems = 2){
  #print(paste("Item Name is ",itemName,sep=""))
  query <- paste("MATCH (s:Session {id:{sessionId}})-[r:HAS_AFFINITY_FOR]->(f:Feature)
                  WITH s, max(abs(r.score)) as max_score
                  MATCH (s)-[r:HAS_AFFINITY_FOR]->(f:Feature)
                  WITH s, f, r.score / max_score as norm_score
                  MATCH (s)-[:LAST_CHOICE]->(:Food)-->(f)<--(m:Food)
                  WITH m.name as menuItem, (sum((norm_score-1)^2) / count(f.name)) as mse
                  WITH menuItem, mse, rand() as rand
                  RETURN menuItem
                  ORDER by mse, rand
                  LIMIT ", maxItems, sep="");
  results<-cypher(graph, query, sessionId=session$id);  
  names(results)<-c("name");
  results;
}

getTopOptionInfo <- function(graph, session, maxItems=5){
  query<-paste("MATCH (s:Session {id:{sessionId}})-[r:HAS_AFFINITY_FOR]->(f:Feature)
                WITH s, max(abs(r.score)) as max_score
                MATCH (s)-[r:HAS_AFFINITY_FOR]->(f:Feature)
                WITH s, f, r.score / max_score as norm_score
                MATCH (s)-[:LAST_CHOICE]->(:Food)-->(f)<-[a:HAS_FEATURE]-(m:Food)
                WITH m.name as menuItem, sum(norm_score*a.strength) as score, 
                (sum((norm_score-a.strength)^2) / count(f.name)) as mse
                WITH menuItem, score, mse, rand() as rand
                RETURN menuItem, score, mse, rand
                ORDER by score desc, mse, rand
               LIMIT ", maxItems, sep="");
  results<-cypher(graph, query, sessionId=session$id);  
  results;
}

getTopFeatureInfo <- function(graph, session, maxItems=5){
  query<-paste("MATCH (s:Session {id:{sessionId}})-[r:HAS_AFFINITY_FOR]->(f:Feature)
                WITH s, max(abs(r.score)) as max_score
                MATCH (s)-[r:HAS_AFFINITY_FOR]->(f:Feature)
                RETURN f.name as feature_name, r.score / max_score as norm_score
                ORDER by norm_score desc
                LIMIT ", maxItems, sep="");
  
  results<-cypher(graph, query, sessionId=session$id);  
  results;
}
#---start here
#This is an interesting query - will probably need something like this to normalize affinities
# MATCH (f:Feature)<-[r2:HAS_FEATURE]-(c:Food),
# (s:Session {id:1})-[r:HAS_AFFINITY_FOR]->(f:Feature)
# RETURN c.name as item_name, c.description, sum(r2.strength) as feature_strength, sum(r.score) as affinity_score
# ORDER BY affinity_score desc, c.name

#Calculate normalized feature scores
# MATCH (s:Session {id:2})-[r:HAS_AFFINITY_FOR]->(f:Feature)
# WITH s, max(abs(r.score)) as max_score
# MATCH (s:Session {id:2})-[r:HAS_AFFINITY_FOR]->(f:Feature)
# RETURN s.id as session_id, f.name as feature, r.score / max_score as norm_score
# order by norm_score desc

#get raw affinity scores
# match (s:Session {id:8})-[r:HAS_AFFINITY_FOR]->(f:Feature)
# return s.id as session_id, f.name as feature_name, r.score as affinity_score
# order by r.score desc

#----------This is the big one, but need to figure out how to deal with cases where
# a menu item has a couple of perfect matches, but a lot of unknowns
# MATCH (s:Session {id:2})-[r:HAS_AFFINITY_FOR]->(f:Feature)
# WITH s, max(abs(r.score)) as max_score
# MATCH (s)-[r:HAS_AFFINITY_FOR]->(f:Feature)
# WITH s, f, r.score / max_score as norm_score
# MATCH (:Food {name:"Grilled Pork Chop"})-->(f)<--(m:Food)
# RETURN s.id, m.name, (sum((norm_score-1)^2) / count(f.name)) as mse
# order by mse

#Get list of menu items, ranked by affinity score:
# MATCH (f:Feature)<--(c:Food),
# (s:Session {id:1})-[r:HAS_AFFINITY_FOR]->(f:Feature)
# RETURN c.name as item_name, c.description, sum(r.score) as affinity_score
# ORDER BY affinity_score desc, c.name

#Get related menu items
# MATCH (a:Food {name:"Lemon Drop"})-->(b:Feature)<--(c:Food)
# RETURN DISTINCT a.name, b.name, c.name
# ORDER BY c.name

#Get related menu items, ordered by sum of affinity scores
# MATCH (a:Food {name:"Lemon Drop"})-->(f:Feature)<--(c:Food),
# (s:Session {id:12})-[r:HAS_AFFINITY_FOR]->(f)
# RETURN a.name as choice_name, c.name as related_item_name, sum(r.score) as affinity_score
# ORDER BY a.name, affinity_score desc