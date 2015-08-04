##########################################
# Runs basic PCA with simple scalings on the data
# Scalings used: none, autoscale, meanscale, rangecentering
# Results available in Results/PCA_plots
#
# INPUTS:
# X - data matrix
# dataName - string to append to result files
#
# OUTPUTS:
# Returns nothing
# Generates pdf files with PCA plots for each of listed scalings
#
# Rafal Kural
##########################################

run_pca_basic_scalings <- function(X, dataName) {
  require(matlab)
  # load required scripts
  # assuming parent script is always two directories above this one
  source("../../Toolbox/pca_svd.r")
  source("../../Toolbox/scalings/auto.r")
  source("../../Toolbox/scalings/mncn.r")
  source("../../Toolbox/scalings/rangescale.r")
  #simple scalings
  Xas <- auto(X)
  Xmncn <- mncn(X)
  Xrs <- rangescale(X)
  #noscale
  pca_svd(X, samplenames, CLASS, paste(dataName, "noscale", sep="_"))
  #autoscale
  pca_svd(Xas, samplenames, CLASS, paste(dataName, "autoscale", sep="_"))
  #meancentered
  pca_svd(Xmncn, samplenames, CLASS, paste(dataName, "meancentered", sep="_"))
  #rangescaled
  pca_svd(Xrs, samplenames, CLASS, paste(dataName, "rangescale", sep="_"))
}