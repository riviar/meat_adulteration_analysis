##############################################
# Wrapper script. 
#
# Performs PCA viar Singular Value Decomposition
# Results available as pdf files
# in Results/PCA_plots directory
#                                            
# Rafal Kural                                
##############################################

# clear workspace and open windows
rm(list = ls())
graphics.off()

#set working directory to script directory
workdir <- dirname(sys.frame(1)$ofile)
setwd(workdir)

#load required libraries and scripts
source("../../Toolbox/run_pca_basic_scalings.r")
source("../../Toolbox/load_data_from_file.r")


####### LOAD DATA ###################
fileToLoad = "Exp2_GCMS.csv"
#name of the data that will appear in result file name
dataName = "Exp2_GCMS"

# load chosen file
DATA <- load_data_from_file(fileToLoad)

# extract samples data and classes
X <- DATA$X
CLASS <- DATA$CLASS

# retrieve names of samples
samplenames <- row.names(X)
####### DATA LOAD END ################

###### BASIC PCA #############################
# runs pca and creates PCA plots for first 3 PCs
# for raw data, autoscaled data, meancentered data
# and rangescaled data
run_pca_basic_scalings(X, dataName)

