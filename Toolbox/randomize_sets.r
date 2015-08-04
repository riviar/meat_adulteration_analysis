########################################################
# Creates training and testing sets populated with random samples
# Ensures that training set has representatives of all classes
# 
# INPUTS:
# X - samples data 
# CLASS - vector of samples classes
# ratio - ratio of training-to-test split (default 0.75)
#
# OUTPUTS:
# Returns list:
# Xtrain - training set data
# CLASStrain - training set classes vector
# Xtest - testing set data
# CLASStest - testing set classes vector
#
# Rafal Kural
########################################################

# main function
randomize_sets <- function(X, CLASS, ratio) {
  
  # set missimg values to defaults
  if (missing(ratio)) {
    ratio <- 0.75
  }

  ####### SPLIT X BY CLASSES #######
  
  # extract unique classes
  classTypes <- unique(CLASS)
  
  # create list of sample matrices by classes
  Xlist <- list(X[which(CLASS == classTypes[1]),])
  for (j in 2:length(classTypes)) {
    Xlist[[j]] <- (X[which(CLASS == classTypes[j]),])
  }
  ####### X SPLITTING END ##########
  
  ####### GENERATE TRAINING SET ##########
  # determine target length of test set
  trainLength <- round(ratio * nrow(X))
  
  #initialize variables
  Xtrain = NULL
  CLASStrain = NULL
  
  # fill training set with values
  for (i in 1:trainLength) {
    #set new seed for random numbers based on system time and process id
    set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
    
    # ensure that training set will have samples from all classes
    # it is done by selecting from different class every loop iteration
    # pick class from which a random sample will be selected
    targetClass <- (i %% (length(classTypes))) + 1
    # ensure that this class is not empty
    counter = i
    while(TRUE) {
      # select random ID from target class list
      randomID <- round(runif(1, 1, nrow(Xlist[[targetClass]])))
      # break loop if sample exists
      if (!(is.nan(randomID))) {
        break
      }
      # try to pick from next class
      targetClass <- (counter %% (length(classTypes))) + 1
      counter = counter + 1
    }
      
    # push chosen sample data to training set
    Xtrain <- rbind(Xtrain, Xlist[[targetClass]][randomID,])
      
    # push chosen sample class to tranin set
    CLASStrain <- c(CLASStrain, classTypes[targetClass])
      
    # remove chosen sample from unprocessed data list
    Xlist[[targetClass]] <- Xlist[[targetClass]][-randomID,]
  }
  ####### GENERATE TRAINING SET END ##########
  
  ####### GENERATE TESTING SET ###############
  
  # initialize variables
  Xtest = NULL
  CLASStest = NULL
  
  # iterate over remaining data and push to testing set
  for(i in 1:length(classTypes)) {
    for(j in 1:nrow(Xlist[[i]])) {
      # break and go to next class if this one is empty
      if(is.na(Xlist[[i]][j,1])) {
        break
      }
      # push sample data to test set
      Xtest <- rbind(Xtest, Xlist[[i]][j,])
      # push sample class to test set
      CLASStest <- c(CLASStest, classTypes[i])
    }
  }
  ####### GENERATE TESTING SET END ###########
  
  return(list(Xtrain = Xtrain, CLASStrain = CLASStrain, Xtest = Xtest, CLASStest = CLASStest))
}