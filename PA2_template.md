# Reproducible Research: Peer Assessment 2
Jesin Fahad  
# Health/Economic Consequences in U.S. caused by Storms ans Weather Events

GitHub: [GitHub Explore NOAA Project]
(https://github.com/jesyfax/Peer-graded-Assignment-Course-Project-2-explore-the-NOAA-Storm-Database) 

# 1 - Introduction
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
library("knitr")
library(RColorBrewer)
```

```
## Warning: package 'RColorBrewer' was built under R version 3.3.2
```

```r
opts_chunk$set(echo = TRUE )
opts_chunk$set(cache = TRUE )
opts_chunk$set(out.width='1000px', dpi=200)
```
# 3 - Data Processing

## Loading NOAA data into R


```r
## Copy LinkLoaction for "Storm Data" and Download CSV File
if(!file.exists("./repdata_data_StormData.csv.bz2")){
    file.create("./repdata_data_StormData.csv.bz2")
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  "./repdata_data_StormData.csv.bz2")
}

## Read data as data frame
storm<-read.csv("./repdata_data_StormData.csv.bz2", header=T)
## backing up the original data and the data has to modified
stormunchanged<-storm
summary(storm)
```

```
##     STATE__                  BGN_DATE             BGN_TIME     
##  Min.   : 1.0   5/25/2011 0:00:00:  1202   12:00:00 AM: 10163  
##  1st Qu.:19.0   4/27/2011 0:00:00:  1193   06:00:00 PM:  7350  
##  Median :30.0   6/9/2011 0:00:00 :  1030   04:00:00 PM:  7261  
##  Mean   :31.2   5/30/2004 0:00:00:  1016   05:00:00 PM:  6891  
##  3rd Qu.:45.0   4/4/2011 0:00:00 :  1009   12:00:00 PM:  6703  
##  Max.   :95.0   4/2/2006 0:00:00 :   981   03:00:00 PM:  6700  
##                 (Other)          :895866   (Other)    :857229  
##    TIME_ZONE          COUNTY           COUNTYNAME         STATE       
##  CST    :547493   Min.   :  0.0   JEFFERSON :  7840   TX     : 83728  
##  EST    :245558   1st Qu.: 31.0   WASHINGTON:  7603   KS     : 53440  
##  MST    : 68390   Median : 75.0   JACKSON   :  6660   OK     : 46802  
##  PST    : 28302   Mean   :100.6   FRANKLIN  :  6256   MO     : 35648  
##  AST    :  6360   3rd Qu.:131.0   LINCOLN   :  5937   IA     : 31069  
##  HST    :  2563   Max.   :873.0   MADISON   :  5632   NE     : 30271  
##  (Other):  3631                   (Other)   :862369   (Other):621339  
##                EVTYPE         BGN_RANGE           BGN_AZI      
##  HAIL             :288661   Min.   :   0.000          :547332  
##  TSTM WIND        :219940   1st Qu.:   0.000   N      : 86752  
##  THUNDERSTORM WIND: 82563   Median :   0.000   W      : 38446  
##  TORNADO          : 60652   Mean   :   1.484   S      : 37558  
##  FLASH FLOOD      : 54277   3rd Qu.:   1.000   E      : 33178  
##  FLOOD            : 25326   Max.   :3749.000   NW     : 24041  
##  (Other)          :170878                      (Other):134990  
##          BGN_LOCATI                  END_DATE             END_TIME     
##               :287743                    :243411              :238978  
##  COUNTYWIDE   : 19680   4/27/2011 0:00:00:  1214   06:00:00 PM:  9802  
##  Countywide   :   993   5/25/2011 0:00:00:  1196   05:00:00 PM:  8314  
##  SPRINGFIELD  :   843   6/9/2011 0:00:00 :  1021   04:00:00 PM:  8104  
##  SOUTH PORTION:   810   4/4/2011 0:00:00 :  1007   12:00:00 PM:  7483  
##  NORTH PORTION:   784   5/30/2004 0:00:00:   998   11:59:00 PM:  7184  
##  (Other)      :591444   (Other)          :653450   (Other)    :622432  
##    COUNTY_END COUNTYENDN       END_RANGE           END_AZI      
##  Min.   :0    Mode:logical   Min.   :  0.0000          :724837  
##  1st Qu.:0    NA's:902297    1st Qu.:  0.0000   N      : 28082  
##  Median :0                   Median :  0.0000   S      : 22510  
##  Mean   :0                   Mean   :  0.9862   W      : 20119  
##  3rd Qu.:0                   3rd Qu.:  0.0000   E      : 20047  
##  Max.   :0                   Max.   :925.0000   NE     : 14606  
##                                                 (Other): 72096  
##            END_LOCATI         LENGTH              WIDTH         
##                 :499225   Min.   :   0.0000   Min.   :   0.000  
##  COUNTYWIDE     : 19731   1st Qu.:   0.0000   1st Qu.:   0.000  
##  SOUTH PORTION  :   833   Median :   0.0000   Median :   0.000  
##  NORTH PORTION  :   780   Mean   :   0.2301   Mean   :   7.503  
##  CENTRAL PORTION:   617   3rd Qu.:   0.0000   3rd Qu.:   0.000  
##  SPRINGFIELD    :   575   Max.   :2315.0000   Max.   :4400.000  
##  (Other)        :380536                                         
##        F               MAG            FATALITIES          INJURIES        
##  Min.   :0.0      Min.   :    0.0   Min.   :  0.0000   Min.   :   0.0000  
##  1st Qu.:0.0      1st Qu.:    0.0   1st Qu.:  0.0000   1st Qu.:   0.0000  
##  Median :1.0      Median :   50.0   Median :  0.0000   Median :   0.0000  
##  Mean   :0.9      Mean   :   46.9   Mean   :  0.0168   Mean   :   0.1557  
##  3rd Qu.:1.0      3rd Qu.:   75.0   3rd Qu.:  0.0000   3rd Qu.:   0.0000  
##  Max.   :5.0      Max.   :22000.0   Max.   :583.0000   Max.   :1700.0000  
##  NA's   :843563                                                           
##     PROPDMG          PROPDMGEXP        CROPDMG          CROPDMGEXP    
##  Min.   :   0.00          :465934   Min.   :  0.000          :618413  
##  1st Qu.:   0.00   K      :424665   1st Qu.:  0.000   K      :281832  
##  Median :   0.00   M      : 11330   Median :  0.000   M      :  1994  
##  Mean   :  12.06   0      :   216   Mean   :  1.527   k      :    21  
##  3rd Qu.:   0.50   B      :    40   3rd Qu.:  0.000   0      :    19  
##  Max.   :5000.00   5      :    28   Max.   :990.000   B      :     9  
##                    (Other):    84                     (Other):     9  
##       WFO                                       STATEOFFIC    
##         :142069                                      :248769  
##  OUN    : 17393   TEXAS, North                       : 12193  
##  JAN    : 13889   ARKANSAS, Central and North Central: 11738  
##  LWX    : 13174   IOWA, Central                      : 11345  
##  PHI    : 12551   KANSAS, Southwest                  : 11212  
##  TSA    : 12483   GEORGIA, North and Central         : 11120  
##  (Other):690738   (Other)                            :595920  
##                                                                                                                                                                                                     ZONENAMES     
##                                                                                                                                                                                                          :594029  
##                                                                                                                                                                                                          :205988  
##  GREATER RENO / CARSON CITY / M - GREATER RENO / CARSON CITY / M                                                                                                                                         :   639  
##  GREATER LAKE TAHOE AREA - GREATER LAKE TAHOE AREA                                                                                                                                                       :   592  
##  JEFFERSON - JEFFERSON                                                                                                                                                                                   :   303  
##  MADISON - MADISON                                                                                                                                                                                       :   302  
##  (Other)                                                                                                                                                                                                 :100444  
##     LATITUDE      LONGITUDE        LATITUDE_E     LONGITUDE_    
##  Min.   :   0   Min.   :-14451   Min.   :   0   Min.   :-14455  
##  1st Qu.:2802   1st Qu.:  7247   1st Qu.:   0   1st Qu.:     0  
##  Median :3540   Median :  8707   Median :   0   Median :     0  
##  Mean   :2875   Mean   :  6940   Mean   :1452   Mean   :  3509  
##  3rd Qu.:4019   3rd Qu.:  9605   3rd Qu.:3549   3rd Qu.:  8735  
##  Max.   :9706   Max.   : 17124   Max.   :9706   Max.   :106220  
##  NA's   :47                      NA's   :40                     
##                                            REMARKS           REFNUM      
##                                                :287433   Min.   :     1  
##                                                : 24013   1st Qu.:225575  
##  Trees down.\n                                 :  1110   Median :451149  
##  Several trees were blown down.\n              :   568   Mean   :451149  
##  Trees were downed.\n                          :   446   3rd Qu.:676723  
##  Large trees and power lines were blown down.\n:   432   Max.   :902297  
##  (Other)                                       :588295
```
Defining variables that will be used:

- EVTYPE: Event Type (Tornados, Flood, ....)

- FATALITIES: Number of Fatalities

- INJURIES: Number of Injuries

- PROGDMG: Property Damage

- PROPDMGEXP: Units for Property Damage (magnitudes - K,B,M)

- CROPDMG: Crop Damage

- CROPDMGEXP: Units for Crop Damage (magnitudes - K,BM,B)

e) Unknown symbols in PROPDMGEXP and CROPDMGEXP can be equated to 0.


```r
## Data Conversion: Factor data to character
storm$PROPDMGEXP<-as.character(storm$PROPDMGEXP)
storm$CROPDMGEXP<-as.character(storm$CROPDMGEXP)

## Property and crop damage units and respective column numbers
propcol<-grep("PROPDMGEXP",colnames(storm))
cropcol<-grep("CROPDMGEXP",colnames(storm))

storm[storm$PROPDMGEXP=="",propcol]<-"0"
storm[storm$PROPDMGEXP=="-",propcol]<-"0"
storm[storm$PROPDMGEXP=="?",propcol]<-"0"
storm[storm$PROPDMGEXP=="+",propcol]<-"0"
storm[storm$PROPDMGEXP=="1",propcol]<-"0"
storm[storm$PROPDMGEXP=="2",propcol]<-"0"
storm[storm$PROPDMGEXP=="3",propcol]<-"0"
storm[storm$PROPDMGEXP=="4",propcol]<-"0"
storm[storm$PROPDMGEXP=="5",propcol]<-"0"
storm[storm$PROPDMGEXP=="6",propcol]<-"0"
storm[storm$PROPDMGEXP=="7",propcol]<-"0"
storm[storm$PROPDMGEXP=="8",propcol]<-"0"
storm[storm$PROPDMGEXP=="h",propcol]<-"100"
storm[storm$PROPDMGEXP=="H",propcol]<-"100"
storm[storm$PROPDMGEXP=="K",propcol]<-"1000"
storm[storm$PROPDMGEXP=="m",propcol]<-"1000000"
storm[storm$PROPDMGEXP=="M",propcol]<-"1000000"
storm[storm$PROPDMGEXP=="B",propcol]<-"1000000000"

storm[storm$CROPDMGEXP=="",cropcol]<-"0"
storm[storm$CROPDMGEXP=="?",cropcol]<-"0"
storm[storm$CROPDMGEXP=="2",cropcol]<-"0"
storm[storm$CROPDMGEXP=="k",cropcol]<-"1000"
storm[storm$CROPDMGEXP=="K",cropcol]<-"1000"
storm[storm$CROPDMGEXP=="m",cropcol]<-"1000000"
storm[storm$CROPDMGEXP=="M",cropcol]<-"1000000"
storm[storm$CROPDMGEXP=="B",cropcol]<-"1000000000"

##Data Convertion: Property and crop damage units to numeric
storm$PROPDMGEXP<-as.numeric(storm$PROPDMGEXP)
storm$CROPDMGEXP<-as.numeric(storm$CROPDMGEXP)
## multiplying the unit and cost to get the cost in same unit
storm$PROPDMG<-storm$PROPDMG*storm$PROPDMGEXP
storm$CROPDMG<-storm$CROPDMG*storm$CROPDMGEXP
```
 
###subsetting the data for our analysis

```r
storm.h <- subset(storm, select = c(EVTYPE,FATALITIES,INJURIES ))
storm.e <- subset(storm, select = c(EVTYPE,PROPDMG,CROPDMG))
```

##Summarize Data

```r
##Summarizing the fatalities and injuries
fatalities<- tapply(storm.h$FATALITIES, storm.h$EVTYPE, sum)
fatalitiestop10  <- sort(fatalities,decreasing = TRUE)[1:10]
injuries    <- tapply(storm.h$INJURIES, storm.h$EVTYPE, sum)
injuriestop10  <- sort(injuries,decreasing = T)[1:10]
##summarizing the property and crop damage
propdmg   <- tapply(storm.e$PROPDMG,  storm.e$EVTYPE, sum)
propdmgtop10 <- sort(propdmg,decreasing = T)[1:10]
cropdmg   <- tapply(storm.e$CROPDMG,  storm.e$EVTYPE, sum)
cropdmgtop10 <-sort(cropdmg,decreasing = T)[1:10]
```

##Plot Data

```r
##plotting the Ten most severe types of weather events in terms of human health impacts
par(mfrow = c(1,2 ),mar=c(2,5,2,2), oma = c(3,1,3,1))
  barplot(fatalitiestop10, names.arg=names(fatalitiestop10), cex.names=0.5, las=3, col = heat.colors(10), 
        space=0.1,  offset = 0.1, ylab = "number of fatalities")
barplot(injuriestop10, names.arg=names(injuriestop10), cex.names=0.5, las=3, col = heat.colors(10), 
        space=0.1,  offset = 0.1, ylab = "number of injuries" )
title(main = 
      "Ten most severe types of weather events \n in terms of human health impacts",
      outer = TRUE)
```

<img src="PA2_template_files/figure-html/unnamed-chunk-6-1.png" width="1000px" />


```r
##ploting the Ten most severe types of weather events \n in terms of economic loss
par(mfrow = c(1,2 ), mar = c(7,4,2,0), oma = c(0,0,2,0))
barplot(propdmgtop10, names.arg=names(propdmgtop10), cex.names=0.5, las=3, col = terrain.colors(10), 
        space=0.1,  offset = 0.1, ylab = "property damage in US-dollar")
barplot(cropdmgtop10, names.arg=names(cropdmgtop10), cex.names=0.5, las=3, col = grey.colors(10), 
        space=0.1,  offset = 0.1, ylab = "crop damage in US-dollar" )
title(main = "Ten most severe types of weather events \n in terms of economic loss",
      outer = TRUE)
```

<img src="PA2_template_files/figure-html/unnamed-chunk-7-1.png" width="1000px" />
# Results and Conclusions

### Answering Question 1
As demonstrated by the Graphs, Tornados causes the greatest number of Fatalities and Injuries. 

Specifically in FATALITIES, after Tornados, EXCESSIVE HEAT, FLASH FLOOD and HEAT are the next ones.

Specifically in INJURIES, after tornados we have TSTM WIND, FLOOD and EXCESSIVE HEAT. 

### Answering Question 2
The impact on property and crop damage is different: 

The largest damage on properties are caused by flood.

But drought has the highest impact on crop.

### Conclusions
Based on evidences demonstrated previously, tornados, floods and droughts have priorities to minimize the impact in human and economic costs of Weather Events. Government and people have to be prepared for each type of events. For safety, it's important to population to know what to do during such events.

