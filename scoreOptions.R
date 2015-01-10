#get high scoring options related to a node

getTopOptionInfo <- function(graph, session, maxItems=5){
  print("Querying for top options");
  print(session$sessionId);
  query<-paste("MATCH (s:Session {sessionId:{sessionId}})-[r:HAS_AFFINITY_FOR]->(f:Feature)
                WITH s, max(abs(r.score)) as max_score
                MATCH (s)-[r:HAS_AFFINITY_FOR]->(f:Feature)
                WITH s, f, r.score / max_score as norm_score
                MATCH (s)-[:LAST_CHOICE]->(:Option)-->(f)<-[a:HAS_FEATURE]-(m:Option)
                WITH m.name as name, m.optionId as optionId, sum(norm_score*a.strength) as score, 
                (sum((norm_score-a.strength)^2) / count(f.name)) as mse
                WITH name, optionId, score, mse, rand() as rand
                RETURN name, optionId, score, mse, rand
                ORDER by score desc, mse, rand
               LIMIT ", maxItems, sep="");
  results<-cypher(graph, query, sessionId=session$sessionId);  
  print(results);
  results;
}

getTopFeatureInfo <- function(graph, session, maxItems=5){
  query<-paste("MATCH (s:Session {sessionId:{sessionId}})-[r:HAS_AFFINITY_FOR]->(f:Feature)
                WITH s, max(abs(r.score)) as max_score
                MATCH (s)-[r:HAS_AFFINITY_FOR]->(f:Feature)
                RETURN f.name as feature_name, f.featureId as feature_id, r.score / max_score as norm_score
                ORDER by norm_score desc
                LIMIT ", maxItems, sep="");
  
  results<-cypher(graph, query, sessionId=session$id);  
  results;
}