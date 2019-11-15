#
#	 code to get to total Damage data.
#
library(ggplot2)
truncData <- stormData %>% select(5:8,23:28)
truncData <- truncData %>% 
				mutate(propDamage = case_when (
										PROPDMGEXP=='K' ~ PROPDMG*1000, 
										PROPDMGEXP=='M' ~ PROPDMG*1000000,
										PROPDMGEXP=='B' ~ PROPDMG*10^9,
										TRUE ~ PROPDMG),
					   cropDamage = case_when(
										CROPDMGEXP=='K' ~ CROPDMG*10^3,
										CROPDMGEXP=='M' ~ CROPDMG*10^6,
										CROPDMGEXP=='B' ~ CROPDMG*10^9,
										TRUE ~ CROPDMG)
						)
truncData <- truncData %>% mutate(totalDamage = propDamage + cropDamage)
truncData <- group_by(truncData, EVTYPE)
aggStormData <- aggregate(totalDamage ~ EVTYPE, truncData, FUN=sum) %>% 
				filter(totalDamage > quantile(truncData$totalDamage,.95))

tData <- head(aggStormData[order(aggStormData$totalDamage, decreasing=T),],15)
g <- ggplot()+
	geom_point(tData, mapping=aes(y=EVTYPE, x=totalDamage))
print(g)
