#
#

source("telemetry-functions.R")

dataDir <- "data"

# file details
#-------------------------------------------------------------------
sourceDir <- file.path("sourceData", "FEATURES-2014-2015")

filenames <- list.files(sourceDir, full.names=TRUE) 

oneDF <- read_csv(filenames[1])

# lets have a look at the names
nameLengths = data.frame( name = names(oneDF) )
nameLengths$charCount = nchar( names(oneDF) )

summary(nameLengths)

nameLengths %>% arrange( desc(charCount) ) %>% select(charCount)

# save the file of names 
write_csv(nameLengths, file.path(dataDir, "originalFeatureNames.csv"))

# and the distribution is....
plotCharLengthDistribution( names(oneDF), nBins = 20)

# this file is used to translate original names and shortened names
ss <- read_csv("features-schema-descriptions.csv")

# need to still fix the names to be more R friendly
new_names <- cleanNames(ss$short_name)
new_names

# now the distribution is....
plotCharLengthDistribution(new_names, nBins = 20)

# MUCH better

# how many derived/computed metrics?
table(ss$`Derived?`)

# load a random number of those files into a data.frame
set.seed(12345)
fileList <- sample(filenames, 3) 

telemetryDF <- loadTelemetryFiles(fileList, new_names)

print( sprintf("Sampled %d/%d files", length(fileList), length(filenames)) )

plotCharLengthDistribution(names(telemetryDF), nBins = 20)

#
# Explore the DataFrame
# -------------------------------------------------------

# explore the dataframe
class(telemetryDF)
str(telemetryDF)
summary(telemetryDF)

nrow(telemetryDF)

# identifier columns
idCols <-  c('host', 'process', 'timestamp', 'isAnomaly', 'No_data')

# get the column names
colNames <- names(telemetryDF)

# these are identifiers and not telemetry metrics
metricNames <- sort( colNames[! colNames %in% idCols] )

# Lets plot the distributions
for ( aMetric in metricNames ) {
  print( plotDistribution(telemetryDF, aMetric) )
}

plotDistribution(telemetryDF, metricNames[1])
summary( telemetryDF[, metricNames[1]] )

plotDistribution(telemetryDF, metricNames[58])
summary( telemetryDF[, metricNames[58]] )

plotDistribution(telemetryDF, metricNames[130])
summary( telemetryDF[, metricNames[130]] )


# OK, for anomaly detection or any modeling, you can remove those with NO change in value
gatheredDF <- gather(telemetryDF %>% select(metricNames), "metric", "value" )

head(gatheredDF)

# make a data.frame with metric statistics
metricDF <- gatheredDF %>% 
  group_by(metric) %>% 
  summarise(
    count = n(), 
    mean = mean(value), 
    med = median(value), 
    sd = sd(value), 
    min = min(value), 
    max = max(value),
    Q1 = quantile(value, probs=.25, na.rm=TRUE),
    Q3 = quantile(value, probs=.75, na.rm=TRUE)
  )

head(metricDF)
tail(metricDF)

# those where min == max
metricDF %>% filter( min != max )

# remove those metrics from the list we care about
usefulMetrics <- metricDF %>% filter( min != max )

# how many?
nrow(usefulMetrics)

# check some
for ( aMetric in sort( usefulMetrics$metric ) ) {
  print( plotDistribution(telemetryDF, aMetric, 50) )
}

# filter down to used metrics
useCols <- c( idCols, unique(usefulMetrics$metric))

telemetryDF2 <- telemetryDF %>% filter( No_data == 0 ) %>% select(useCols)

names(telemetryDF2)

# create the time aggregation column
# ------------------------------------

# create new column for the 5 minute bucket....
tBucket <- 5

# create hours/minute features

# first translate the timestamp character column
telemetryDF2$TS <- as.POSIXct( strptime(telemetryDF2$timestamp, "%Y-%m-%d %H:%M") )
summary(telemetryDF2$TS)

# now extract features for date, hour and minute
telemetryDF2$Date <- as.Date( strftime(telemetryDF2$TS, "%Y-%m-%d") )
telemetryDF2$Hour <- as.numeric( strftime(telemetryDF2$TS, "%H") )
telemetryDF2$Minute <- as.numeric( strftime(telemetryDF2$TS, "%M") )

# create a rounded minute bucket
telemetryDF2$Minute5 <- (telemetryDF2$Minute %/% tBucket ) * tBucket

# combine into one feature (this is what we will aggregate on)
telemetryDF2$aggTime <- sprintf("%s %02d:%02d", telemetryDF2$Date, telemetryDF2$Hour, telemetryDF2$Minute5)

telemetryDF2 %>% select(aggTime, TS) %>% arrange( aggTime ) %>% head(10)
  
# columns to group with
groupCols <- c("host", "process", "aggTime")

# could isAnomaly still be used?
useAnomaly <- telemetryDF2 %>% group_by_at( groupCols ) %>%
  summarise(
    count = n(),
    sumAnomaly = sum( isAnomaly == "true" )
  )

table( useAnomaly$sumAnomaly )

# do Things stay anomalous for long then?
t <- useAnomaly %>% filter( sumAnomaly == 5)
t

# first 20
t[1:20,]

# Later....
t[ 217:226,]

# columns to aggregate on
aggCols <- names(telemetryDF2)
aggCols <- aggCols[! aggCols %in% c(groupCols, "timestamp", "isAnomaly", "No_data", "TS", "Date", "Hour", "Minute", "Minute5")]

# the ugly way.....
telemetryDF3 <- telemetryDF2 %>% 
  group_by( host, process, aggTime ) %>%
  summarize( 
    Active_connections_source09_JDBCConnectionPoolRuntime_mean = mean( Active_connections_source09_JDBCConnectionPoolRuntime ),
    Active_connections_source09_JDBCDataSourceRuntime_mean = mean( Active_connections_source09_JDBCDataSourceRuntime ),
    Active_connections_source10_JDBCConnectionPoolRuntime_mean = mean( Active_connections_source10_JDBCConnectionPoolRuntime ),
    Active_connections_source10_JDBCDataSourceRuntime_mean = mean( Active_connections_source10_JDBCDataSourceRuntime ),
    Available_db_connection_activity_source02_JDBCConnectionPoolRuntime = mean( Available_db_connection_activity_source02_JDBCConnectionPoolRuntime )
    )

# now have stuff aggregated in the 5 minute window
telemetryDF3 <- telemetryDF2 %>% 
  group_by_at( groupCols ) %>%
  summarize_at( aggCols, c("min", "mean", "max"), na.rm = TRUE )

names(telemetryDF3)
nrow(telemetryDF3)

# aggregating columns.....
sources <- c(1, 2, 3, 4, 5, 6, 8, 9, 10)
agg1Template <- "Connection_delay_source%02d_JDBCConnectionPoolRuntime_min"
agg2Template <- "Connection_delay_source%02d_JDBCDataSourceRuntime_min"

agg1Cols <- sprintf( agg1Template, sources)
agg2Cols <- sprintf( agg2Template, sources)

# lets add the columns
telemetryDF4 <- telemetryDF3 %>%  
  mutate(
    Connection_delay_source_JDBCConnectionPoolRuntime_min_sum = select(., agg1Cols) %>% rowSums(),
    Connection_delay_source_JDBCDataSourceRuntime_min_sum = select(., agg2Cols) %>% rowSums()
  )

# by itself
telemetryDF3 %>% select( agg1Cols ) %>% rowSums

# lets add ungroup then add the columns
telemetryDF4 <- telemetryDF3 %>%  
  ungroup %>%
  mutate(
    Connection_delay_source_JDBCConnectionPoolRuntime_min_sum = select(., agg1Cols) %>% rowSums(),
    Connection_delay_source_JDBCDataSourceRuntime_min_sum = select(., agg2Cols) %>% rowSums()
    )

# same number rows?
nrow(telemetryDF3) == nrow(telemetryDF4)

# check the values
telemetryDF4 %>% select( c("Connection_delay_source_JDBCConnectionPoolRuntime_min_sum", agg1Cols ) )
telemetryDF4 %>% select( c("Connection_delay_source_JDBCDataSourceRuntime_min_sum", agg2Cols ) )

nrow(telemetryDF4)
names(telemetryDF4)

# save into a file to check with excel (to really check your logic)
ss1 <- telemetryDF4 %>% select( c("Connection_delay_source_JDBCConnectionPoolRuntime_min_sum", agg1Cols ) )
ss2 <- telemetryDF4 %>% select( c("Connection_delay_source_JDBCDataSourceRuntime_min_sum", agg2Cols ) )

# can confirm in excel with these data dumps
write_csv(ss1, "check1.csv")
write_csv(ss2, "check2.csv")

