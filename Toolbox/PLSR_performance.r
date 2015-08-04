#####################################################
# Function that tests PLS regression
#
# INPUTS:
# X - matrix of samples numerical data
# CLASS - vector of samples classes
# ratio - training/test set ratio
# maxncomp - maximum number of latent variables to consider
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

PLSR_performance <- function(X, CLASS, ratio, maxncomp, iterations, allowedDeviation) {
  require(plsdepot)
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
    cat(paste("PLS-R iteration: ", i, "..\n", sep = ""))
    
    # split data into training and test sets
    randomizedSets <- randomize_sets(X = X, CLASS = CLASS, ratio = ratio)
    Xtrain = randomizedSets$Xtrain
    CLASStrain = randomizedSets$CLASStrain
    Xtest = randomizedSets$Xtest
    CLASStest = randomizedSets$CLASStest
    
    # create model and predict test classes
    model <- plsreg1(Xtrain, CLASStrain, comps = maxncomp, crosval = TRUE)
    regressionCoefficients <- as.matrix(model$reg.coefs[-1])
    regressionIntercept <- model$reg.coefs[1]
    predictedValues <- as.matrix(Xtest) %*% regressionCoefficients + regressionIntercept
    
    # vectors creating boundary of "successfull" classification
    upperSuccessCLASS <- CLASStest + allowedDeviation
    bottomSuccessCLASS <- CLASStest - allowedDeviation
    
    # count how many samples were predicted correctly
    #hitCount <- length(which((as.vector(model$predclass) >= bottomSuccessCLASS) & 
    #                    (as.vector(model$predclass) <= upperSuccessCLASS)))
    #hitCount <- length(which((as.vector(model) == bottomSuccessCLASS))) + 
    #  length(which((as.vector(model) == upperSuccessCLASS))) + 
    #  length(which((as.vector(model) == CLASStest))) 
    
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