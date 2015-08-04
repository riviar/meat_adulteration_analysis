######################################################
# Saves all data provided into csv files
#
# INPUTS:
# datasets - list where each element is an output of 
# algorithm analysing script
# datasetsNames - vector of names for datasets
#
# OUTPUTS: 
# Returns nothing
# Creates csv files with data for each dataset
# in directory /Results/Performance_Raw_Data/
#
# Rafal Kural
######################################################

save_results_csv <- function(datasets, datasetsNames) {
  
  # create directory for outputs (does nothing if those exist)
  outputDir = "Results"
  dir.create(path = outputDir, showWarnings = FALSE)
  outputDir = "Results/Performance_Raw_Data"
  dir.create(path = outputDir, showWarnings = FALSE)
  
  # merge data from datasets into type-specific matrices
  for(i in 1:length(datasets)) {
    write.csv(x = datasets[[i]], file = paste(outputDir, "/", datasetsNames[i], ".csv", sep = ""))
  }
}
