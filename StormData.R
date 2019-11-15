library(dplyr)
library(ggplot2)
library(gridExtra)

# stormData <- read.csv("repdata-data-StormData.csv", stringsAsFactors = FALSE)

shortData <- stormData %>% select(6:8,23:28)
shortData <- shortData %>% mutate(propDamage=PROPDMG * ifelse(PROPDMGEXP == 'K',1000,
                                                              ifelse(PROPDMGEXP == 'M',1000000,0)))

summEVTYPE <- shortData %>% group_by(EVTYPE)

summEVTYPE <- aggregate(FATALITIES ~ EVTYPE, summEVTYPE, FUN=sum)

summEVTYPE <- subset(summEVTYPE, FATALITIES > 0)

orderedEVTYPE <- summEVTYPE[order(summEVTYPE$FATALITIES, decreasing=TRUE),]
orderedEVTYPE <- subset(orderedEVTYPE, FATALITIES > quantile(orderedEVTYPE$FATALITIES,.95))
cat("Event with highest fatalities: ") 
cat(orderedEVTYPE$EVTYPE[1])
g <- ggplot(orderedEVTYPE)
g <- g+geom_point(mapping=aes(y=EVTYPE, x=FATALITIES))
print(g)
