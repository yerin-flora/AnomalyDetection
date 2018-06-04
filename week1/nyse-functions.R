#
# NYSE Data Functions
#

library(tidyverse)
library(tidyquant)

#'
#'  function to create a dataframe with symbol and contents of the file
#'
#' @param path path of the filename
#' @param filename the filename to load
#' @param symbol the symbol being loaded
#' 
loadSymbolfile <- function( path, filename, symbol = NULL ) {
  
  # if a null symbol, get from the filename
  if ( is.null(symbol) ) {
    symbol <- strsplit(filename, '.', fixed = TRUE)[[1]][1]
  }
  
  theFile <- file.path(path, filename)
  
  if ( file.size(theFile) == 0) {
    return(NULL)
  }
  
  #  sDF <- read_csv(file = theFile)
  sDF <- read.csv(file = theFile, stringsAsFactors=FALSE)
  
  if (is.null(sDF) == FALSE && nrow(sDF) > 0) {
    sDF$Symbol = toupper(symbol)
  }
  
  sDF
}

#'
#' Read read all the symbolFiles in readDir and 
#' output contents with symbol in the schema to destFile
#' 
#' @param destFile file to write all contents to (will be replaced)
#' @param readDir directory of symbol files to read
#'
combineSymbolFiles <- function( destFile, readDir ) {
  readFiles <- list.files(readDir)
  
  if (file.exists(destFile)) {
    file.remove(destFile)
  }
  
  for (f in readFiles) {
    print(sprintf("Loading: %s ...", f))
    
    aDF <- loadSymbolfile(readDir, f)
    
    if (is.null(aDF) || nrow(aDF) == 0) {
      print("...skipping, empty")
      next
    }
    
    # if the file exists, will append and NOT write column names
    appendToFile = file.exists(destFile)
    
    # CANNOT use write.csv with append :(
    write.table(aDF, file = destFile, sep=",", row.names=FALSE, append = appendToFile, col.names = !appendToFile)
    #    write_csv(aDF, path = destFile, append = appendToFile, col_names = !appendToFile)
    
    print("Done")
  }
  
  destFile
}

#'
#' Plot the given data
#' plots HLOC and will facet the plots by Symbol
#' 
#' Its assumed the data.frame has the names: High, Low, Open Close, Data, Symbol
#' 
plotBarchart <- function( data ) {

  # plot them
  data %>%
    ggplot(aes(x = Date, y = Close)) +
    geom_barchart(aes(open = Open, high = High, low = Low, close = Close)) +
    facet_wrap( ~ Symbol ) +
    scale_y_continuous( labels = scales::comma ) +
    labs(title = "High/Low/Open/Close", y = "Closing Price", x = "Date") + 
    theme_tq()
}

#'
#' plots Candlesticks and will facet the plots by Symbol
#' 
#' Its assumed the data.frame has the names: High, Low, Open Close, Data, Symbol
#' 
plotCandlesticks <- function( data ) {
  data %>%
    ggplot(aes(x = Date, y = Close)) +
    geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
    facet_wrap( ~ Symbol ) +
    scale_y_continuous( labels = scales::comma ) +
    labs(title = "High/Low/Open/Close", y = "Closing Price", x = "Date") + 
    theme_tq()

}
