######################################################
# Generate performance plots for all datasets provided
#
# INPUTS:
# datasets - list where each element is an output of 
# algorithm analysing script
# datasetsNames - vector of names for datasets
#
# OUTPUTS: 
# Returns nothing
# Generates plots of mean accuracy scores, 
# mean RMSEPs and mean max errors in directory
# /Results/Performance_Plots/
#
# Rafal Kural
######################################################

generate_plots <- function(datasets, datasetsNames) {
  source("../../Toolbox/generate_performance_plot.r")
  
  # initialize merged-by-type matrices
  mergedAccuracyMeanScores <- NULL
  mergedRMSEPMeanScores <- NULL
  mergedMaxErrorMeanScores <- NULL
  
  # merge data from datasets into type-specific matrices
  for(i in 1:length(datasets)) {
    # push accuracy mean scores to merged matrix
    mergedAccuracyMeanScores <- rbind(mergedAccuracyMeanScores, datasets[[i]]$accuracyScoresMeans)
    mergedRMSEPMeanScores <- rbind(mergedRMSEPMeanScores, datasets[[i]]$rmsepValuesMeans)
    mergedMaxErrorMeanScores <- rbind(mergedMaxErrorMeanScores, datasets[[i]]$largestErrorMeans)
  }
  
  # create directory for outputs (does nothing if those exist)
  outputDir = "Results"
  dir.create(path = outputDir, showWarnings = FALSE)
  outputDir = "Results/Performance_Plots"
  dir.create(path = outputDir, showWarnings = FALSE)
  
  # call generating script to generate all plots
  # accuracy
  generate_performance_plot(data = mergedAccuracyMeanScores*100, plotName = "Accuracy Means", 
                            dataNames = algorithmNames, 
                            file = paste(outputDir, "/Accuracy_Means.pdf", sep = ""))
  
  # RMSEP
  generate_performance_plot(data = mergedRMSEPMeanScores, plotName = "RMSEP Means", 
                            dataNames = algorithmNames, 
                            file = paste(outputDir, "/RMSEP_Means.pdf", sep = ""))
  
  # Max Error
  generate_performance_plot(data = mergedMaxErrorMeanScores, plotName = "Max Error Means", 
                            dataNames = algorithmNames, 
                            file = paste(outputDir, "/Max_Error_Means.pdf", sep = ""))
}
