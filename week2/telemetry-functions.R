#
# Functions for the Features Dataset
#

require(tidyverse)

#'
#' Convenience function to plot a distribution
#' 
#' @param aDF data.frame data to use for the plot
#' @param colName name of the columns who's distribution you want plotted
#' @param nBins number of bins to plot
#' 
plotDistribution <- function(aDF, colName, nBins = 100) {
  
  # Lets plot the distributions
  p <- ggplot(data = aDF) +
    aes_string(x = sprintf("%s", colName) ) + 
    geom_histogram( bins = nBins, fill = "blue", alpha = .75 ) +
    labs(title = colName, x = colName, y = "Count")
  p
  
}

#'
#' Convenience function to plot the name length distribution of a data.frame
#' 
#'  @param dataFrame the dataframe 
#'  @param numBins the number of bins for the distribution
#'  
plotCharLengthDistribution <- function( colNames, nBins = 10 ) {
  # lets have a look at the names
  nameLengths = data.frame( name = colNames )
  nameLengths$charCount = nchar( colNames )
  
  nameLengths %>% arrange( desc(charCount) )
  
  plotDistribution(nameLengths, "charCount", nBins = nBins)
}

#'
#' removes bad characters from the list of strings
#' 
#' @param orgNames array of strings who's names are to be adjusted
#' 
cleanNames <- function( orgNames ) {
  newNames <- gsub(" ", "_", orgNames)
  newNames <- gsub(":", "_", newNames)
  newNames <- gsub("/", "_div_", newNames)
  newNames <- gsub("-", "_", newNames)
  newNames <- gsub("\\.", "_", newNames)
  newNames <- gsub("___", "_", newNames)
  newNames <- gsub("__", "_", newNames)
  
  newNames
}

#'
#' Will load the list of filenames into one data.frame
#' 
#' @param fileList a list of filenames to load
#' @param newNames if not null, will replace loaded names with these names (assumed to align in order)
#' 
#'
loadTelemetryFiles <- function( fileList, newNames = NULL ) {
  df <- data.frame()
  
  # all files into one
  for (f in fileList) {
    print(sprintf("Loading: %s ...", f))
    
    aDF <- read.csv(file = f, stringsAsFactors = FALSE)
    
    if (is.null(aDF) || nrow(aDF) == 0) {
      print("...skipping, empty")
      next
    }
    
    if ( is.null(newNames) == FALSE && length(newNames) == length(names(aDF)) ) {
      names(aDF) <- newNames  
    }

    df <- rbind(df, aDF)
    
    print("Done")
  }
  
  df
}