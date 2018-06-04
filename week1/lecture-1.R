#
# Slide code snippets
#

source('nyse-functions.R')

require(lubridate)

# load a file and get the schema
df <- read_csv("sourceData/Stocks/gs.us.txt")

head(df)

# BIG VOlume on 5/4/1999, why?

# Summary statistics
summary(df)

# max volume is huge, lets explore that
ggplot(data = df) +
  aes(x = Volume ) + 
  geom_histogram( bins = 100, fill = "blue", alpha = .75 ) + 
  scale_x_continuous( labels = scales::comma )

# log scale helps 
ggplot(data = df) +
  aes(x = log(Volume) ) + 
  geom_histogram( bins = 100, fill = "blue", alpha = .75 ) + 
  scale_x_continuous( labels = scales::comma ) 


# long tail, what's there?
df %>% filter( Volume > 100000000 )

# Google the dates
# https://lauder.wharton.upenn.edu/wp-content/uploads/2015/06/Chronology_Economic_Financial_Crisis.pdf 
# https://www.google.com/search?q=goldman+4%2F16%2F2010&rlz=1C1OKWM_enUS782US782&oq=goldman+4%2F16%2F2010&aqs=chrome..69i57.14657j0j7&sourceid=chrome&ie=UTF-8
# https://www.sec.gov/news/press/2010/2010-59.htm 

# what does the volume look like around those dates?
kDate <- as.Date("2008-09-17")
wNum <- 2

df %>% 
  filter( Date >= kDate - weeks(wNum) & Date <= kDate + weeks(wNum)) %>% 
  ggplot() + 
  geom_line( aes(x=Date, y=Volume) ) + 
  geom_vline( xintercept = kDate, color = "blue" ) + 
  scale_y_continuous( labels = scales::comma ) 


# First signs of outliers!
# first spike would be an outlier, why?
# is the second similar value also an outlier?

df <- read_csv("data/stocks.csv")
summary(df)

# negative Low prices?
df %>% filter( Low < 0 )

# one day, is that a mistake?
kDate <- as.Date("2017-08-10")
wNum <- 1

df %>% filter( Symbol == "HLG" ) %>% 
  filter( Date >= kDate - weeks(wNum) & Date <= kDate + weeks(wNum))

df %>% filter( Symbol == "HLG" ) %>% 
  filter( Date >= kDate - weeks(wNum) & Date <= kDate + weeks(wNum)) %>% 
  plotBarchart()

# load a file and check it
df1 <- read_csv("sourceData/Stocks/hlg.us.txt")
df1 %>% filter( Date >= kDate - weeks(wNum) & Date <= kDate + weeks(wNum))

# OK, this should be fixed


# Billions?
# BIG numbers, summarize Symbols?
df %>% 
  filter( Open > 1000000000) %>%
  group_by(Symbol) %>%
  summarise(
    Num = n()
  )

dDF <- df %>% filter( Open > 1000000000)
dDF

dDF %>% summarize(
  min(dDF$Date),
  max(dDF$Date)
)

summary(dDF)

# plot them
plotBarchart( df %>% filter( Symbol == "DRYS") ) +
  coord_x_date( xlim = c( min(dDF$Date), max(dDF$Date) ) ) + 
  geom_hline( yintercept = 1000000000, color = "blue" ) 
  
# Splits
splits <- 25*4*15*8*4*7*5*7

4.18 / splits

#
#
#

#
# Transformation: change from previous
# ----------------------------------------------------------------

library(tidyquant)
library(RcppRoll)

df2 <- stockDF %>% 
  arrange(Symbol, Date) %>%
  group_by(Symbol) %>%
  mutate(
    Open_Change = Open - lag(Open),
    High_Change = High - lag(High),
    Low_Change = Low - lag(Low),
    Close_Change = Close - lag(Close),
    Volume_Change = Volume - lag(Volume),
    
    Open_PctChange = Open_Change / lag(Open) * 100,
    High_PctChange = High_Change / lag(High) * 100,
    Low_PctChange = Low_Change / lag(Low) * 100,
    Close_PctChange = Close_Change / lag(Close) * 100,
    Volume_PctChange = Volume_Change - lag(Volume) * 100,
    
    Open_Mean30 = roll_mean( Open, 30, fill=0 ),
    High_Mean30 = roll_mean( High, 30, fill=0 ),
    Low_Mean30 = roll_mean( Low, 30, fill=0 ),
    Close_Mean30 = roll_mean( Close, 30, fill=0 ),
    Volume_Mean30 = roll_mean( Volume, 30, fill=0 )
  )

names(df2)



# distributions of Close_PctChange
pDF <- df2 %>% filter( Symbol %in% c('GS', 'MS', 'C') )

p <- ggplot(data = pDF) +
  aes(x = Close_PctChange ) + 
  geom_histogram( bins = 100, fill = "blue", alpha = .75 )
p

# wow! big % changes, when?
summary(pDF$Close_PctChange)
pDF %>% filter( Close_PctChange > 50 )

kDate <- as.Date("2008-10-13")
pDF %>% 
  filter( Symbol == "MS") %>%
  filter( Date >= kDate - weeks(1) & Date <= kDate + weeks(1)) %>% 
  plotBarchart() + 
  geom_vline( xintercept = kDate, color = "blue", linetype = "dotted" )

kDate <- as.Date("2008-11-24")
pDF %>% 
  filter( Symbol == "C") %>%
  filter( Date >= kDate - weeks(1) & Date <= kDate + weeks(1)) %>% 
  plotBarchart() + 
  geom_vline( xintercept = kDate, color = "blue", linetype = "dotted" )

#
# Intresting points
# both occured over a weekend in 2008 (financial crisis)

# Sp500 ETFs
sp500ETF <- c("SPY", "IVV", "VOO")

# 
etfDF %>% filter (Symbol %in% sp500ETF) %>% summary

# SP500 information
sp500Members <- read.csv(text = getURL(sp500URL))

sp500DF <- stockDF %>% filter (Symbol %in% sp500Members$Symbol)

summary(sp500DF)

# join the SP500 information
sp500DF <- sp500DF %>% left_join( sp500Members, by = "Symbol" )

names(sp500DF)
head(sp500DF)

# Changes in the day
# - absolute
# - percentage (from open)
sp500DF$Change_OpenClose <- sp500DF$Close - sp500DF$Open # positive change is price went UP
sp500DF$PctChange_OpenClose <- sp500DF$Change_OpenClose / sp500DF$Open

# distributions of PCT daychange
p <- ggplot(data = sp500DF) +
  aes(x = log(PctChange_OpenClose) ) + 
  geom_histogram( bins = 100, fill = "blue", alpha = .75 )
p


# Randomly choose 20 ETFs
allETFs <- etfDF %>% distinct(Symbol)

set.seed(12345)
etfSample <- sample(allETFs$Symbol, 20) 


