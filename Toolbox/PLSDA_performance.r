#####################################################
# Function that tests PLS-DA classification
#
# X - matrix of samples numerical data
# CLASS - vector of samples classes
# ratio - training/test set ratio
# ncomp - vector containing number of components to use in PLS-DA
# iterations - how many times repeat the test
# allowProximity - TRUE to treat predicting class adjecent to observed as a success, 
# FALSE - only direct hits count, default FALSE
# crossval - number of "folds" to use in cross validation
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

PLSDA_performance <- function(X, CLASS, ratio, ncomp, iterations, allowProximity, crossval) {
  require(plsgenomics)
  source("../../Toolbox/randomize_sets.r")
  
  # set allowProximity to FALSE if missing
  if (missing(allowProximity)) {
    allowProximity <- FALSE
  }
  
  # initialize monitoring variables
  accuracyScores <- 0
  accuracyScoresMeans <- 0
  largestErrorValues <- 0
  largestErrorMeans <- 0
  rmsepValues <- 0
  rmsepValuesMeans <- 0
  
  for(i in 1:iterations) {
  
    # print current iteration
    cat(paste("PLS-DA iteration: ", i, "..\n", sep = ""))
    
    # split data into training and test sets
    randomizedSets <- randomize_sets(X = X, CLASS = CLASS, ratio = ratio)
    Xtrain = randomizedSets$Xtrain
    CLASStrain = randomizedSets$CLASStrain
    Xtest = randomizedSets$Xtest
    CLASStest = randomizedSets$CLASStest
    
    # create model
    model <- pls.lda(Xtrain = Xtrain, Ytrain = CLASStrain, Xtest = Xtest, ncomp = ncomp ,nruncv = crossval)
    
    # count how many samples were predicted correctly
    if (allowProximity) {
      # vectors creating boundary of "successfull" classification (+/- 10% of observed)
      upperSuccessCLASS <- CLASStest + 10
      bottomSuccessCLASS <- CLASStest - 10
      
      hitCount <- length(which((as.vector(model$predclass) == bottomSuccessCLASS))) + 
        length(which((as.vector(model$predclass) == upperSuccessCLASS))) + 
        length(which((as.vector(model$predclass) == CLASStest))) 
    }
    if (allowProximity == FALSE) {
      hitCount <- length(which((as.vector(model$predclass) == CLASStest)))
    }
    
    # calculate accuracy percentage and round to 2 decimal places
    accuracy <- round((hitCount/length(CLASStest)), 4)
    # simply push accuracy to scores vector
    accuracyScores[i] = accuracy
    # calculate mean of all accuracy scores up to now and push it to scores mean vector
    accuracyScoresMeans[i] = round(mean(accuracyScores), 4)
    
    # calculate RMSEP and round to 4 decimal places
    rmsep <- round((sum(((as.numeric(as.vector(model$predclass)) - CLASStest)^2)^(1/2)) / length(CLASStest)), 4)
    # push to rmsep values vector
    rmsepValues[i] <- rmsep
    # calculate mean of rmsep values up to now and push it to rmsep mean values vector
    rmsepValuesMeans[i] <- round(mean(rmsepValues), 4)
    
    # calculate max error and push it to max error values vector
    maxError <- max(((as.numeric(as.vector(model$predclass)) - CLASStest)^2)^(1/2))
    largestErrorValues[i] <- maxError
    # calculate mean of max errors up to now and push to max error mean values vector
    largestErrorMeans[i] <- round(mean(largestErrorValues), 4)
  }
  
  # return accuracy scores, rmsep values and largest errors
  return(list(accuracyScores = accuracyScores, accuracyScoresMeans = accuracyScoresMeans,
              rmsepValues = rmsepValues, rmsepValuesMeans = rmsepValuesMeans, 
              largestErrorValues = largestErrorValues, largestErrorMeans = largestErrorMeans))
}