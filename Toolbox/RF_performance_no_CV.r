#####################################################
# Function that tests RF regression without cross validation
# As cross validation in RF is extremely long for large datasets
# this algorithm is another option that gives generally good results.
# Number of trees is hard-coded to 100
#
# INPUTS:
# X - matrix of samples numerical data
# CLASS - vector of samples classes
# ratio - training/test set ratio
# iterations - how many times repeat the test
# allowedDeviation - by how much can score deviate from observed value to count as success
#
# OUTPUTS:
# accuracyScores - vector of accuracy scores obtained for each iteration
# accuracyScoresMeans - vector of obtained accuracy mean scores for each iteration
# rmsepValues - vector of RMSEP values obtained for each iteration
# rmsepValuesMeans - vector of mean RMSEP values for each iteration
# largestErrorValues - vector of largest errors that occured for each iteration
# largestErrorMeans - vector of means of largest  errors for each iteration
#
# Rafal Kural
#####################################################

RF_performance <- function(X, CLASS, ratio, iterations, allowedDeviation) {
  require(randomForest)
  source("../../Toolbox/randomize_sets.r")
  
  # initialize monitoring variables
  accuracyScores <- 0
  accuracyScoresMeans <- 0
  largestErrorValues <- 0
  largestErrorMeans <- 0
  rmsepValues <- 0
  rmsepValuesMeans <- 0
  
  for(i in 1:iterations) {
    
    # print current iteration
    cat(paste("RF iteration: ", i, "..\n", sep = ""))
    
    # split data into training and test sets
    randomizedSets <- randomize_sets(X = X, CLASS = CLASS, ratio = ratio)
    Xtrain = randomizedSets$Xtrain
    CLASStrain = randomizedSets$CLASStrain
    Xtest = randomizedSets$Xtest
    CLASStest = randomizedSets$CLASStest
    
    # train random forest model using optimal ntree
    model <- randomForest(x = Xtrain, y = CLASStrain, xtest = Xtest, ytest = CLASStest, ntree = 100)
    
    # extract predicted values
    predictedValues <- model$test$predicted
    
    # vectors creating boundary of "successfull" classification
    upperSuccessCLASS <- CLASStest + allowedDeviation
    bottomSuccessCLASS <- CLASStest - allowedDeviation
    
    #none of above worked well, so..
    hitCount <- 0
    for(j in 1:length(predictedValues)) {
      if (predictedValues[j] >= bottomSuccessCLASS[j] &&
          predictedValues[j] <= upperSuccessCLASS[j]) {
        hitCount = hitCount + 1
      }
    }
    
    # calculate accuracy percentage and round to 2 decimal places
    accuracy <- round((hitCount/length(CLASStest)), 4)
    
    # simply push accuracy to scores vector
    accuracyScores[i] = accuracy
    # calculate mean of all accuracy scores up to now and push it to scores mean vector
    accuracyScoresMeans[i] = round(mean(accuracyScores), 4)
    
    # calculate RMSEP and round to 4 decimal places
    rmsep <- round((sum(((as.numeric(as.vector(predictedValues)) - CLASStest)^2)^(1/2)) / length(CLASStest)), 4)
    # push to rmsep values vector
    rmsepValues[i] <- rmsep
    # calculate mean of rmsep values up to now and push it to rmsep mean values vector
    rmsepValuesMeans[i] <- round(mean(rmsepValues), 4)
    
    # calculate max error and push it to max error values vector
    maxError <- max(((as.numeric(as.vector(predictedValues)) - CLASStest)^2)^(1/2))
    largestErrorValues[i] <- maxError
    # calculate mean of max errors up to now and push to max error mean values vector
    largestErrorMeans[i] <- round(mean(largestErrorValues), 4)
  }
  
  # return accuracy scores, rmsep values and largest errors
  return(list(accuracyScores = accuracyScores, accuracyScoresMeans = accuracyScoresMeans,
              rmsepValues = rmsepValues, rmsepValuesMeans = rmsepValuesMeans, 
              largestErrorValues = largestErrorValues, largestErrorMeans = largestErrorMeans))
}