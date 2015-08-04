####################################################################
# Performs pca via SVD                                              
# Generates pdf file with PCA plots of first 3 PCs                  
# 
# INPUTS:
# X - input data matrix                                             
# samplenames - matrix with names of samples
# CLASS - class vector for samples
# append_filename - what to append at the end of filename for plots 
# saves pca plots one directory above in Results/PLA_Plots         
# 
# OUTPUTS:
# Returns nothing
# Generates pdf file with PCA plots
#
# Rafal Kural                                                       
####################################################################

pca_svd <- function (X, samplenames, CLASS, append_filename) {
  source("../../Toolbox/pca_plots_pdf.r")
  
  #svd PCA
  PCA <- prcomp(X, retX=T, center=F, scale=F)
  #extract scores
  SCORES2 <- PCA$x
  Wraw <- PCA$sdev^2
  
  # create directory for outputs (does nothing if it exists)
  outputDir = "Results"
  dir.create(path = outputDir, showWarnings = FALSE)
  outputDir = "Results/PCA_Plots"
  dir.create(path = outputDir, showWarnings = FALSE)
  
  pca_plots_pdf(SCORES2, samplenames, CLASS, Wraw, paste(outputDir, "/PCA_SVD_PLOTS_", append_filename, sep=""))
  }