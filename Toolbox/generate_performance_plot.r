##################################################
# Generates pdf file with performance plot 
#
# INPUTS:
# data - matrix of data to plot
# plotName - string describing the type of plot
# dataNames - vector of names describing input data elements
# file - output file
#
# OUTPUTS:
# Returns nothing
# Generates pdf file containing performance plot
#
# Rafal Kural
##################################################

generate_performance_plot <- function(data, plotName, dataNames, file) {
  require(colorRamps)
  
  # generate different colors for every data entry
  colors <- primary.colors(nrow(data), steps = 3, no.white = TRUE)
  
  # create pdf file
  pdf(file)
  
  # prepare plot drawing area
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  xAxisPreset <- c(0, ncol(data))
  yAxisPreset <- c(min(data), max(data))
  plot(xAxisPreset, yAxisPreset, xlab = "Iterations", 
       ylab = "Score", main = plotName, type = "n")
  # add steps to axis
  #axisStep <- (max(data) - min(data)) / 10
  #axis(side=2, at=seq(min(data), max(data), by = axisStep))
  
  for(i in 1:nrow(data)) {
    lines(1:ncol(data), data[i,], col = colors[i])
  }
  
  # generate legend for plot
  legend("topright", inset=c(-0.42,0), dataNames, lty=c(1), col=colors, cex = 0.8)
  
  # close file
  dev.off()
}