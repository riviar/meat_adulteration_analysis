###########################################
# Extracts numerical data and samples class from file
# Keeps samples and column names in X variable
#
# File needs to have headers and first row needs 
# to be filled with samples names
# Class columns needs to have "Class" header
#
# INPUTS:
# filepath - path to the file
#
# OUTPUTS:
# Returns list of data:
# X - numerical data 
# Class - samples classes
#
# Rafal Kural
###########################################

load_data_from_file <- function(filepath) {
  # load chosen file
  DATA <- read.table(filepath, sep = ",", header = TRUE, row.names = 1)
  
  # columns to ignore when extracting wavelength data
  fieldsToIgnore <- c("Class")
  
  # extract data
  X <- DATA[,!(names(DATA) %in% fieldsToIgnore)]
  
  # extract class vector
  CLASS <- as.vector(DATA$Class)
  
  # change data type to numeric
  CLASS <- as.numeric(CLASS)
  
  return(list(X = X, CLASS = CLASS))
}