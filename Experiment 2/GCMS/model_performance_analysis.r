#################################################
# Wrapper script running parallel analysis
# for a group of recognition algorithms.
#
# INPUTS:
# none
#
# OUTPUTS:
# Returns nothing
# Generates series of files documenting performance
# of recognition models
#
# Classification algorithms:
# 1. Support Vector Machines (SVM, krnls ponylomial and radial)
# 2. Partial Least Squares Discriminant Analysis (PLS-DA)
# Regression algorithms:
# 3. Random Forest (RF)
# 4. Partial Least Squares Regression (PLS-R)
# 
# Accuracy criteria:
# Exp1:
# Classification: direct
# Regression: +/- 0.5 in score
# Exp2:
# Classification: +/- 10% in class
# Regression: +/- 10 in score
#
# Rafal Kural
#################################################

# clear workspace and open windows
rm(list = ls())
graphics.off()

#set working directory to script directory
workdir <- dirname(sys.frame(1)$ofile)
setwd(workdir)

#loading libraries and required scripts
library(foreach)
library(doMC)
source("../../Toolbox/load_data_from_file.r")
source("../../Toolbox/PLSDA_performance.r")
source("../../Toolbox/PLSR_performance.r")
source("../../Toolbox/SVM_performance.r")
source("../../Toolbox/RF_performance.r")
source("../../Toolbox/generate_plots.r")
source("../../Toolbox/save_results_csv.r")

####### ANALYSIS SETTINGS ###########
# ratio of training-to-testing set (0.75 recommended)
dataSplitRatio = 0.75

# number of iterations for analysis (>50 recommended)
iterations = 100

# allows prediction of class adjecent to observed to be treated as correct result
# classification models only
# (TRUE recommended if many classes are present)
allowClassProximity = TRUE

# allowed score deviation for regression models to consider prediction to be correct
allowedRegrScoreDeviation = 10
####### ANALYSIS SETTINGS END #######

####### MODELS SETTINGS #############
# please note that all models are being ran with cross validation

# 1.SVM settings
# number of "folds" in cross validation
crossValFoldSVM <- 20

# 2. PLS-DA
# number of "folds" in cross validation
crossValFoldPLSDA <- 20
# vector with number of latent variables to consider
ncompVector <- c(1, 20:40)

# 3. RF
# number of "folds" in cross validation
crossValFoldRF <- 20

# 4. PLS-R
# maximum number of latent variables to consider
maxncomp <- 40
####### MODELS SETTINGS END #########

####### LOAD DATA ###################
fileToLoad = "Exp2_GCMS.csv"
#name of the data that will appear in result file name
dataName = "Exp2_GCMS"

# load chosen file
DATA <- load_data_from_file(fileToLoad)

# extract samples data and classes
X <- DATA$X
CLASS <- DATA$CLASS
####### DATA LOAD END ################

####### ANALYSIS MAIN BODY ###########
# number of CPU cores to use
registerDoMC(5)

# little hack to run separate algorithms in parallel
foreach(i = 1:5) %dopar% {
  if(i == 1) {
    #SVM polynomial analysis
    SVMpolynResults <- SVM_performance(X = X, CLASS = CLASS, ratio = dataSplitRatio, krnl = "polynomial",
                                       iterations = iterations, allowProximity = allowClassProximity,
                                       crossval = crossValFoldSVM)
  } else if (i == 2) {
    #SVM radial analysis
    SVMradialResults <- SVM_performance(X = X, CLASS = CLASS, ratio = dataSplitRatio, krnl = "radial",
                                       iterations = iterations, allowProximity = allowClassProximity,
                                       crossval = crossValFoldSVM)
  } else if (i == 3) {
    #PLS-DA analysis
    PLSDAresults <- PLSDA_performance(X = X, CLASS = CLASS, ratio = dataSplitRatio, ncomp = ncompVector,
                                      iterations = iterations, allowProximity = allowClassProximity,
                                      crossval = crossValFoldPLSDA)
  } else if (i == 4) {
    #RF analysis
    RFresults <- RF_performance(X = X, CLASS = CLASS, ratio = dataSplitRatio, iterations = iterations, 
                                allowedDeviation = allowedRegrScoreDeviation, crossval = crossValFoldRF)
  } else if (i == 5) {
    #PLS-R analysis
    PLSRresults <- PLSR_performance(X = X, CLASS = CLASS, ratio = dataSplitRatio, maxncomp = maxncomp,
                                    iterations = iterations, allowedDeviation = allowedRegrScoreDeviation)
  }
}
####### ANALYSIS MAIN BODY END ########

####### GENERATE PLOTS ################
# vector with analysis data names
algorithmNames <- c("SVM Polynomial", "SVM Radial", "PLS-DA", "RF", "PLS-R")
# push all analysis results into a list
resultsList <- list(SVMpolynResults, SVMradialResults, PLSDAresults, RFresults, PLSRresults)
generate_plots(datasets = resultsList, datasetsNames = algorithmNames)
####### GENERATE PLOTS END ############

####### SAVE ALL RESULTS ##############
save_results_csv(datasets = resultsList, datasetsNames = algorithmNames)
####### SAVE ALL RESULTS END ##########