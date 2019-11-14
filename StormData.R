library(dplyr)
library(ggplot2)
library(gridExtra)

#stormData <- read.csv("repdata_data_StormData.csv", stringsAsFactors = FALSE)
#
shortData <- stormData %>% select(6:8,23:28)
shortData <- stormData %>% 
  mutate(propDamage = case_when (
                        PROPDMGEXP=='K' ~ PROPDMG*1000, 
                        PROPDMGEXP=='M' ~ PROPDMG*1000000,
                        PROPDMGEXP=='B' ~ PROPDMG*10^9,
                        TRUE ~ PROPDMG),
         cropDamage = case_when(
                        CROPDMGEXP=='K' ~ CROPDMG*10^3,
                        CROPDMGEXP=='M' ~ CROPDMG*10^6,
                        CROPDMGEXP=='B' ~ CROPDMG*10^9,
                        TRUE ~ CROPDMG),
         totalDamage = propDamage + cropDamage)

summEVTYPE <- shortData %>% group_by(EVTYPE)

aggStormData_Fatality <- aggregate(FATALITIES ~ EVTYPE, summEVTYPE, FUN=sum)
aggStormData_Fatality <- subset(aggStormData_Fatality, FATALITIES > 0)
aggStormData_Fatality <- aggStormData_Fatality[order(aggStormData_Fatality$FATALITIES, decreasing=TRUE),]
aggStormData_Fatality <- subset(aggStormData_Fatality, FATALITIES > quantile(aggStormData_Fatality$FATALITIES,.95))

aggStormData_Economic <- aggregate(totalDamage ~ EVTYPE, summEVTYPE, FUN=sum)
aggStormData_Economic <- subset(aggStormData_Economic, totalDamage > 0)
aggStormData_Economic <- aggStormData_Economic[order(aggStormData_Economic$totalDamage, decreasing=TRUE),]

aggStormData_Economic <- subset(aggStormData_Economic, totalDamage > quantile(aggStormData_Economic$totalDamage, .98))

g1 <- ggplot(aggStormData_Fatality)+geom_point(mapping=aes(y=EVTYPE, x=FATALITIES))
print(g1)

g2 <- ggplot()+geom_point(aggStormData_Economic, mapping=aes(y=EVTYPE, x=totalDamage)) + ylab("")
print(g2)