#solutionCheck.R
#Check to see if we've identified a valid solution to know when to stop asking questions

doesValidSolutionExistBasic <- function(cycle){
  if (cycle > 5) {
    return(TRUE);
    }
  else {
    return(FALSE);
    }
}

doesValidSolutionExist <- function(topOptions, cycle){
  if (cycle < 3) {return(FALSE)};
  if (cycle > 10) {return(TRUE)};
  
  #Pull the score and mse values for the top-10 options for this cycle
  scoreValues <- topOptions$score;
  mseValues <- topOptions$mse;
  
  #Calculate mean and standard deviation of scores for this cycle
  #print(scoreValues);
  meanScore<-mean(scoreValues);
  sdScore<-sd(scoreValues);
  
  #Calculate 95% (2 sd) threshold value 
  thresholdValue<-meanScore + (2 * sdScore);
   print(paste("Top Value is ", scoreValues[1], sep=""));
   print(paste("95% Threshold Value is ", thresholdValue, sep=""));
  if (scoreValues[1] > thresholdValue) { 
#     print("Value greater than threshold");
#     print(paste("Top MSE is ", mseValues[1], sep=""));
    if (mseValues[1] < 0.1){
#       print("MSE is less then 10%")
      #If top value is more than 2 sd above mean, and mse is less than 10%, we have a winner!
      return(TRUE);
    }  
  }
  return(FALSE);
}