# Reproducible Research: Peer Assessment 2
Jesin Fahad  
# Health/Economic Consequences in U.S. caused by Storms ans Weather Events

GitHub: [GitHub Explore NOAA Project]
(https://github.com/jesyfax/Peer-graded-Assignment-Course-Project-2-explore-the-NOAA-Storm-Database) 

## 1 - Introduction
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This Data Science Project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.


# 2 - Synopsis
This report address questions related to Weather Events and Storms in U.S. that are most damaging in terms of Fatalities, Injuries and damages to properties and crop.

The two questions to be answered are:

1.Which types of events are most harmful with respect to population health?

2.Which types of events have the greatest economic consequences?

## Loading required libraries


```r
getwd()
```

```
## [1] "C:/Users/Admin/Desktop/PA 2"
```

```r
library(knitr)
library(markdown)
```

```
## Warning: package 'markdown' was built under R version 3.3.2
```

```r
library(rmarkdown)
```

```
## Warning: package 'rmarkdown' was built under R version 3.3.2
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.2
```

```r
library(stats)
```

##Loading NOAA data into R


```r
## Copy LinkLoaction for "Storm Data" and Download CSV File
if(!file.exists("./repdata_data_StormData.csv.bz2")){
    file.create("./repdata_data_StormData.csv.bz2")
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  "./repdata_data_StormData.csv.bz2")
}

## Read data as data frame
storm<-read.csv("./repdata_data_StormData.csv.bz2", header=T)
```


##Analysing the Data

Defining variables that will be used:

- EVTYPE: Event Type (Tornados, Flood, ....)

- FATALITIES: Number of Fatalities

- INJURIES: Number of Injuries

- PROGDMG: Property Damage

- PROPDMGEXP: Units for Property Damage (magnitudes - K,B,M)

- CROPDMG: Crop Damage

- CROPDMGEXP: Units for Crop Damage (magnitudes - K,BM,B)


```r
varsNedeed <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm <- storm[varsNedeed]
dim(storm)
```

```
## [1] 902297      7
```

```r
names(storm)
```

```
## [1] "EVTYPE"     "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP"
## [6] "CROPDMG"    "CROPDMGEXP"
```

```r
str(storm)
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```

###Calculating Total for Property Damage 

* Refactor of variable PROPDNGEXP


```r
unique(storm$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
storm$PROPDMGEXP <- mapvalues(storm$PROPDMGEXP, from = c("K", "M","", "B", "m", "+", "0", "5", "6", "?", "4", "2", "3", "h", "7", "H", "-", "1", "8"), to = c(10^3, 10^6, 1, 10^9, 10^6, 0,1,10^5, 10^6, 0, 10^4, 10^2, 10^3, 10^2, 10^7, 10^2, 0, 10, 10^8))
storm$PROPDMGEXP <- as.numeric(as.character(storm$PROPDMGEXP))
storm$PROPDMGTOTAL <- (storm$PROPDMG * storm$PROPDMGEXP)/1000000000
```

* Refactor of variable CROPDMGEXP variable


```r
unique(storm$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
storm$CROPDMGEXP <- mapvalues(storm$CROPDMGEXP, from = c("","M", "K", "m", "B", "?", "0", "k","2"), to = c(1,10^6, 10^3, 10^6, 10^9, 0, 1, 10^3, 10^2))
storm$CROPDMGEXP <- as.numeric(as.character(storm$CROPDMGEXP))
storm$CROPDMGTOTAL <- (storm$CROPDMG * storm$CROPDMGEXP)/1000000000
```

# Processing the Data:

Lets answer the question about Which Types of Events are most Harmful for population HEALTH? The variables involved are FATALITIES and INJURIES.


##Events are most harmful to population Health?

Reference link: [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

### *Total Number of Fatalities per Event


```r
sumFatalities <- aggregate(FATALITIES ~ EVTYPE, data = storm,  FUN="sum")
dim(sumFatalities) 
```

```
## [1] 985   2
```

* Ordering Number of Fatalities and defining the top 10 Weather events


```r
fatalities10events <- sumFatalities[order(-sumFatalities$FATALITIES), ][1:10, ]
dim(fatalities10events)
```

```
## [1] 10  2
```

```r
fatalities10events
```

```
##             EVTYPE FATALITIES
## 834        TORNADO       5633
## 130 EXCESSIVE HEAT       1903
## 153    FLASH FLOOD        978
## 275           HEAT        937
## 464      LIGHTNING        816
## 856      TSTM WIND        504
## 170          FLOOD        470
## 585    RIP CURRENT        368
## 359      HIGH WIND        248
## 19       AVALANCHE        224
```

* BarPlot of the 10 Fatalities Events most harmful to population Health.


```r
par(mfrow = c(1,1), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(fatalities10events$FATALITIES, names.arg = fatalities10events$EVTYPE, las = 3, main = "10 Fatalities Highest Events", ylab = "Number of Fatalities")
```

![](PA2_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
dev.copy(png, "fatalities-events.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


### *Total Number of Injuries per Event

Using the same reasoning of Fatalities, let's evaluate the Number of Injuries per type of Event (EVTYPE)


```r
sumInjuries <- aggregate(INJURIES ~ EVTYPE, data = storm,  FUN="sum")
dim(sumInjuries)  
```

```
## [1] 985   2
```

* Ordering Number of INJURIES and defining the top 10 Weather events in this category


```r
injuries10events <- sumInjuries[order(-sumInjuries$INJURIES), ][1:10, ]
dim(injuries10events)
```

```
## [1] 10  2
```

```r
injuries10events
```

```
##                EVTYPE INJURIES
## 834           TORNADO    91346
## 856         TSTM WIND     6957
## 170             FLOOD     6789
## 130    EXCESSIVE HEAT     6525
## 464         LIGHTNING     5230
## 275              HEAT     2100
## 427         ICE STORM     1975
## 153       FLASH FLOOD     1777
## 760 THUNDERSTORM WIND     1488
## 244              HAIL     1361
```

* BarPlot of the 10 INJURIES Events most harmful to population Health.


```r
par(mfrow = c(1,1), mar = c(12, 6, 3, 2), mgp = c(4, 1, 0), cex = 0.8)
barplot(injuries10events$INJURIES, names.arg = injuries10events$EVTYPE, las = 3, main = "10 Injuries Highest Events", ylab = "Number of Injuries")
```

![](PA2_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
dev.copy(png, "injuries-events.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


## Which type of Events have the greatest Economic consequences? 

The item 2.7 (page 12 and the APPENDIX B) of National Weather Service Storm Data documentation describes about Damage. Two variables, PROPDMG (for Property Damage) and CROPDMG (for Crop Damage) are used to represent these losts. If you want, read more about theses damages, please connect with National Weather Service using the link [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)


### *Property Damage

* Calculating Property Damage for type of Event


```r
sumPropertyDamage <- aggregate(PROPDMGTOTAL ~ EVTYPE, data = storm,  FUN="sum")
dim(sumPropertyDamage) 
```

```
## [1] 985   2
```

* We have 985 observations, which is a great number of Events to present in a Plot. 
* Lets stay with the 10 most Property Damage Events


```r
propdmg10Total <- sumPropertyDamage[order(-sumPropertyDamage$PROPDMGTOTAL), ][1:10, ]
propdmg10Total
```

```
##                EVTYPE PROPDMGTOTAL
## 170             FLOOD   144.657710
## 411 HURRICANE/TYPHOON    69.305840
## 834           TORNADO    56.947381
## 670       STORM SURGE    43.323536
## 153       FLASH FLOOD    16.822674
## 244              HAIL    15.735268
## 402         HURRICANE    11.868319
## 848    TROPICAL STORM     7.703891
## 972      WINTER STORM     6.688497
## 359         HIGH WIND     5.270046
```

* BarPlot of the 10 Proprietary Damage Events most harmful to population economic.


```r
par(mfrow = c(1,1), mar = c(12, 6, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(propdmg10Total$PROPDMGTOTAL, names.arg = propdmg10Total$EVTYPE, las = 3, main = "10 Property Damages Highest Events", ylab = "Damage Property Values (in Billions)")
```

![](PA2_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
dev.copy(png, "propdmg-total.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

###* Crop Damage

* Calculating Crop Damage for type of Event


```r
sumCropDamage <- aggregate(CROPDMGTOTAL ~ EVTYPE, data = storm,  FUN="sum")
dim(sumCropDamage)
```

```
## [1] 985   2
```

* We have 985 observations, which is a great number of Events to present in a Plot. 
* Lets stay with the 10 most Crop Damage Events


```r
cropdmg10Total <- sumCropDamage[order(-sumCropDamage$CROPDMGTOTAL), ][1:10, ]
cropdmg10Total
```

```
##                EVTYPE CROPDMGTOTAL
## 95            DROUGHT    13.972566
## 170             FLOOD     5.661968
## 590       RIVER FLOOD     5.029459
## 427         ICE STORM     5.022113
## 244              HAIL     3.025954
## 402         HURRICANE     2.741910
## 411 HURRICANE/TYPHOON     2.607873
## 153       FLASH FLOOD     1.421317
## 140      EXTREME COLD     1.292973
## 212      FROST/FREEZE     1.094086
```

* BarPlot of the 10 Crop Damage Events most harmful to population economic.


```r
par(mfrow = c(1,1), mar = c(10, 6, 3, 2), mgp = c(3, 1, 0), cex = 0.6)
barplot(cropdmg10Total$CROPDMGTOTAL, names.arg = cropdmg10Total$EVTYPE, las = 2, main = "10 Crop Damages Highest Events", ylab = "Damage Crop Values (in Billions) ")
```

![](PA2_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
dev.copy(png, "cropdmg-total.png", width = 480, height = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


# Results and Conclusions

### Answering Question 1
As demonstrated by the Graphs, Tornados causes the greatest number of Fatalities and Injuries. 

Specifically in FATALITIES, after Tornados, EXCESSIVE HEAT, FLASH FLOOD and HEAT are the next ones.

Specifically in INJURIES, after tornados we have TSTM WIND, FLOOD and EXCESSIVE HEAT. 

### Answering Question 2
Floods are the Weather Event that cause most Property Damage, followed by Hurrucanes.

Drought are the Weather Event that causes most Crop damages, follwed by Flood.

### Conclusions
Based on evidences demonstrated previously, tornados, floods and droughts have priorities to minize the impact in human and economic costs of Weather Events. Government and society have to be alert and prepared for each type of events. For safety, it's important to population to know what to do during these events.
