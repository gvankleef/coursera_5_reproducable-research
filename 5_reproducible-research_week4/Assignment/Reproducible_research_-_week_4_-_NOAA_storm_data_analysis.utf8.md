---
title: Weather events causing the most damage to the economy & public health in the
  US  between 1995 and 2011
author: "Guido van Kleef"
date: "13 june 2019"
output:
  html_document: default
  pdf_document: default
---
##Synopsis
In this report we aim to find which weather events are the most harmful to the public health and economy in the United States between the years 1995 and 2011. We obtained data from the National Climatic Data Center (NCDC) who receive the storm related data from the National Weather Service (NWS). We specifically obtained data between the years 1995 and 2011 since there are more recent measurements and we are especailly interested in recents & recurring events, because these are more predictable and it's easier to come up with policies to reduce the health & economical damage caused by these events. From these data, we found that, Tornado's are by far the most harmful to both the publich health and economy. However, on average extreme heat is doing more damage to public health per incident and hurricanes are on average doing more damage to the economy, but these events are occuring less often than Tornado's so the overall impact is lower.


##Data Processing

###Download & read data
1. Load the libraries needed for the analysis
2. Download the data to a local directory & then read it into an R object: rawdata. 

```r
library(downloader)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(tidyr)

if(!dir.exists("./data")){dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("./data/stormdata.csv.bz2")){download.file(url, destfile = "./data/stormdata.csv.bz2")}
unzip("./data/stormdata.csv.bz2", exdir = "./data")
```

```
## Warning in unzip("./data/stormdata.csv.bz2", exdir = "./data"): error 1 in
## extracting from zip file
```

```r
rawdata <- read.csv("./data/stormdata.csv.bz2")
```

###Prepare data
1. Only selecting data between 1995 and 2011.
2. Clean up the rawdata file by removing rows that only contain NA values
3. Create columns for the crop and property damage expressed in absolute numbers. 
In the original data file the numbers (PROPDMG and CROPDMG) are expressed by the units in the
columns (PROPDMGEXP & CROPDMGEXP).

```r
cleaned_data <- rawdata %>%
        filter(as.Date(as.character(BGN_DATE),"%d/%m/%Y") >= as.Date("01/01/1995","%d/%m/%Y")) %>%
        mutate(propdmg = case_when(is.na(PROPDMG) ~ 0,
                                   PROPDMGEXP == "K" ~ PROPDMG * 1000, 
                                   PROPDMGEXP == "M" ~ PROPDMG * 1000000),
               cropdmg = case_when(CROPDMGEXP == "K" ~ CROPDMG * 1000,
                                   CROPDMGEXP == "M" ~ CROPDMG * 1000000,
                                   is.na(CROPDMG) ~ 0)) %>%
        mutate(propdmg = replace(propdmg, is.na(propdmg),0),
               cropdmg = replace(cropdmg, is.na(cropdmg),0)) %>%
        filter_all(any_vars(!is.na(.)))
```


##Results

Top 10 events causing the most fatalities and injuries

```r
agg_victims_tot <- cleaned_data %>%
        group_by(EVTYPE) %>%
        summarise(fatalities = sum(FATALITIES), 
                  injuries = sum(INJURIES), 
                  total_victims = sum(FATALITIES) + sum(INJURIES),
                  avg_victims = mean(FATALITIES) + mean(INJURIES)) %>%
        arrange(desc(total_victims)) %>%
        mutate_each(funs(prettyNum(., big.mark=",")))
```

```
## Warning: funs() is soft deprecated as of dplyr 0.8.0
## please use list() instead
## 
##   # Before:
##   funs(name = f(.))
## 
##   # After: 
##   list(name = ~ f(.))
## This warning is displayed once per session.
```

```r
top_10 <- head(agg_victims_tot,10)

top_10_data <- cleaned_data %>%
        filter(EVTYPE %in% unique(top_10$EVTYPE)) %>%
        group_by(year = floor_date(as.Date(as.character(BGN_DATE),"%d/%m/%Y"),"year"),
                 EVTYPE) %>%
        summarise(total_victims = sum(FATALITIES) + sum(INJURIES))

ggplot(top_10_data, aes(x = year, y = total_victims)) +
        geom_line() +
        facet_wrap(.~EVTYPE, scales = "free") +
        labs(title = "Top 10 weather events causing the most victims (injuries & fatalities) in the US since 1995", x = "Year", y = "Total Victims")
```

<img src="Reproducible_research_-_week_4_-_NOAA_storm_data_analysis_files/figure-html/top 10 events fatalities & injuries-1.png" width="672" />

```r
print(head(agg_victims_tot,10))
```

```
## # A tibble: 10 x 5
##    EVTYPE            fatalities injuries total_victims avg_victims
##    <chr>             <chr>      <chr>    <chr>         <chr>      
##  1 TORNADO           552        7,991    8,543         0.8907309  
##  2 EXCESSIVE HEAT    967        2,710    3,677         4.630982   
##  3 LIGHTNING         285        1,703    1,988         0.3482221  
##  4 HEAT              739        1,153    1,892         5.821538   
##  5 TSTM WIND         96         1,350    1,446         0.02818768 
##  6 WINTER STORM      100        726      826           0.17463    
##  7 FLASH FLOOD       373        444      817           0.03815617 
##  8 HIGH WIND         97         493      590           0.07375    
##  9 THUNDERSTORM WIND 43         483      526           0.01579153 
## 10 HAIL              2          384      386           0.004407298
```


Top 10 events causing the most total economical damage

```r
agg_eco_dmg_tot <- cleaned_data %>%
        group_by(EVTYPE) %>%
        summarise(propdmg_costs = sum(propdmg), 
                                 cropdmg_costs = sum(cropdmg),
                                 total_costs = sum(propdmg)+sum(cropdmg),
                                 avg_costs = mean(propdmg) + mean(cropdmg)) %>%
        arrange(desc(total_costs)) %>%
        mutate_each(funs(prettyNum(., big.mark=",")))

top_10 <- head(agg_eco_dmg_tot,10)

top_10_data <- cleaned_data %>%
        filter(EVTYPE %in% unique(top_10$EVTYPE)) %>%
        group_by(year = floor_date(as.Date(as.character(BGN_DATE),"%d/%m/%Y"),"year"),
                 EVTYPE) %>%
        summarise(total_costs = sum(propdmg)+sum(cropdmg))

ggplot(top_10_data, aes(x = year, y = total_costs)) +
        geom_line() +
        facet_wrap(.~EVTYPE, scales = "free") +
        labs(title = "Top 10 weather events causing the most economical damage in the US since 1995", x = "Year", y = "Total Costs")
```

<img src="Reproducible_research_-_week_4_-_NOAA_storm_data_analysis_files/figure-html/top 10 events economical damage-1.png" width="672" />

```r
print(head(agg_eco_dmg_tot,10))
```

```
## # A tibble: 10 x 5
##    EVTYPE            propdmg_costs  cropdmg_costs  total_costs    avg_costs
##    <chr>             <chr>          <chr>          <chr>          <chr>    
##  1 FLOOD             11,536,282,250 4,444,005,500  15,980,287,750 1,447,359
##  2 DROUGHT           1,045,589,000  11,868,301,000 12,913,890,000 5,783,202
##  3 TORNADO           9,693,051,960  151,379,500    9,844,431,460  1,026,424
##  4 HAIL              5,504,376,670  1,170,607,600  6,674,984,270  76,214.11
##  5 FLASH FLOOD       5,409,049,320  593,307,300    6,002,356,620  280,326.8
##  6 HURRICANE         3,334,469,010  791,230,000    4,125,699,010  48,537,6…
##  7 THUNDERSTORM WIND 2,432,066,880  124,773,000    2,556,839,880  76,761.23
##  8 ICE STORM         2,332,991,500  9,830,000      2,342,821,500  2,659,275
##  9 TSTM WIND         1,463,438,360  280,031,900    1,743,470,260  33,986.44
## 10 WILDFIRE          1,399,422,770  228,718,500    1,628,141,270  1,241,908
```
